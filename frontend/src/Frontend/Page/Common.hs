{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Common
( -- * useful combinators
  unwrapJSON
, prettyJSON
, voidMaybe
  -- * rendering pact types
, renderMetaData
, renderPayload
, renderYield
, renderPactExec
, renderProvenance
, renderRichObject
, renderContinuation
  -- * linking txs
, transactionsLink
) where


------------------------------------------------------------------------------
import Control.Lens
import Control.Monad (unless, when)
------------------------------------------------------------------------------
import Data.Aeson as A
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX

-- ------------------------------------------------------------------------ --
-- Pact modules

import qualified Pact.Types.ChainId as Pact
import Pact.Types.Continuation
import Pact.Types.Names
import Pact.Types.Term (FieldKey(..), ObjectMap(..), PactId(..))
import Pact.Types.Pretty

-- ------------------------------------------------------------------------ --
-- Chainweb modules

import Chainweb.Api.ChainId
import Chainweb.Api.Hash
import Chainweb.Api.Payload
import ChainwebData.TxSummary

-- ------------------------------------------------------------------------ --
-- Reflex modules

import Common.Route
import Common.Types
import Common.Utils

import Frontend.App
import Frontend.Common
import Frontend.Page.Types

import Obelisk.Route
import Obelisk.Route.Frontend

import Reflex.Dom.Core hiding (Value)



-- | Do nothing or apply a function to the 'Just' clause of a 'Maybe'
-- in some monad.
voidMaybe :: Monad m => (a -> m ()) -> Maybe a -> m ()
voidMaybe = maybe (pure ())

-- | Pretty print a pact name
--
ppName :: Name -> Text
ppName (Name (BareName n _)) = n
ppName (QName (QualifiedName (ModuleName t ns) n _)) =
    maybe "" (\(NamespaceName nsn) -> nsn <> ".") ns <> t <> "." <> n
ppName (DName dn) = tshow dn

unwrapJSON :: Value -> Text
unwrapJSON = \case
    Object o -> "{ " <> (T.intercalate " , " (toList $ imap keyvalue o)) <> " }"
    Array a -> "[" <> (T.intercalate "," (toList $ fmap unwrapJSON a) ) <> "]"
    String s -> s
    Null -> "null"
    Number n -> tshow n
    Bool b  -> if b then "true" else "false"
  where
    keyvalue k v = k <> " : " <> unwrapJSON v



-- | Link a 'BlockHash' to its transactions endpoint
--
transactionsLink
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => NetId
  -> ChainId
  -> Hash
  -> m ()
transactionsLink netId c bhash =
    routeLink route $ text $ hashHex bhash
  where
    route = addNetRoute netId (unChainId c)
      $ Chain_BlockHash :/ (hashB64U bhash, Block_Transactions :/ ())

-- | Render 'PollMetaData' and link to its block information
--
renderMetaData
    :: ( MonadApp r t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Prerender js t m
       )
    => NetId
    -> ChainId
    -> Maybe Value
      -- ^ Value ~ 'PollMetaData'
    -> m ()
renderMetaData _ _ Nothing = text "None"
renderMetaData _ _ (Just A.Null) = text ""
renderMetaData netId cid (Just v) = case fromJSON v of
    Success (PollMetaData bh bt bhash phash) -> do
      detailsSection $ do
        tfieldLeaf "Block Height" $ text $ tshow bh
        tfieldLeaf "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
        tfieldLeaf "Block Hash" $ transactionsLink netId cid bhash
        tfieldLeaf "Parent Hash" $ transactionsLink netId cid phash
    A.Error e -> text $ "Unable to decode metadata: " <> T.pack e

-- | Render an object as a structured table, instead of raw json
--
renderRichObject
    :: MonadApp r t m
    => Object
    -> m ()
renderRichObject m
    | HM.null m = pure ()
    | otherwise = void
      $ detailsSection
      $ HM.traverseWithKey go m
  where
    go label v = tfieldLeaf label $ text $ unwrapJSON v

-- | Render the 'Payload' of a 'Transaction'
--
renderPayload
    :: MonadApp r t m
    => Payload
    -> m ()
renderPayload = \case
    ExecPayload (Exec _ d) -> do
      detailsSection $
        voidMaybe (tfieldPre "Data" . text . prettyJSON) d
    ContPayload (Cont pid rb step d p) -> do
      detailsSection $ do
        tfieldLeaf "Pact Id" $ text pid
        tfieldLeaf "Rollback" $ text $ tshow rb
        tfieldLeaf "Step" $ text $ tshow step
        tfieldLeaf "Data" $ text $ unwrapJSON d
        tfieldLeaf "Cont Proof" $ text $ fromMaybe "" p

-- | Render a 'PactExec' object in a 'CommandResult'
--
renderPactExec
    :: MonadApp r t m
    => RouteToUrl (R FrontendRoute) m
    => SetRoute t (R FrontendRoute) m
    => Prerender js t m
    => PactExec
    -> NetId
    -> Either Text [TxSummary]
    -> m ()
renderPactExec (PactExec stepCount y x step (PactId pid) pcont rb) netId res =
    detailsSection $ do
      tfieldLeaf "Step Count" $ text $ tshow stepCount
      voidMaybe (tfield "Yield" . renderYield) y
      voidMaybe (tfieldLeaf "Executed" . text . tshow) x
      tfieldLeaf "Step" $ text $ tshow step
      tfieldLeaf "Pact Id" $ text pid
      tfield "Continuation" $ renderContinuation pcont
      tfieldLeaf "Rollback" $ text $ tshow rb
      tfieldLeaf "Next Step" $ case res of
        Left err -> tfield "Error" $ text err
        Right xs -> case span ((== TxSucceeded) . _txSummary_result) xs of
          (ys,zs) -> do
            unless (null ys) $ do
              text "Successful:"
              el "br" blank
              iforM_ ys $ \i next -> do
                txDetailLink $ _txSummary_requestKey next
                when (i < length ys - 1) $ el "br" blank
            el "br" blank
            unless (null zs) $ do
              text $ "Failed:"
              el "br" blank
              iforM_ (take 10 zs) $ \i z -> do
                txDetailLink $ _txSummary_requestKey z
                when (i < length zs - 1) $  el "br" blank
              when (not $ null $ drop 10 zs) $ text "...and more"
  where
    txDetailLink rk = routeLink (mkTxDetailRoute rk) $ text rk
    mkTxDetailRoute rk = mkNetRoute netId $ NetRoute_TxDetail :/ rk

-- | Render the 'Provenance' pact type
--
renderProvenance
    :: MonadApp r t m
    => Provenance
    -> m ()
renderProvenance (Provenance (Pact.ChainId c) mhash) =
    detailsSection $ do
      tfieldLeaf "Target Chain" $ text c
      tfieldLeaf "Module Hash" $ text $ tshow mhash

-- | Render 'Yield' pact type
--
renderYield
    :: MonadApp r t m
    => Yield
    -> m ()
renderYield (Yield (ObjectMap m) p mCid) =
    detailsSection $ do
      voidMaybe (tfieldLeaf "Source Chain" . text . Pact._chainId) mCid
      tfield "Data" $ renderRichObject $ yieldMap m
      voidMaybe (tfield "Provenance" . renderProvenance) p
  where
    yieldMap = HM.fromList
      . fmap (\(FieldKey k, v) -> (k, toJSON v))
      . M.toList

-- | Render the 'PactContinuation' pact type
--
renderContinuation
    :: MonadApp r t m
    => PactContinuation
    -> m ()
renderContinuation (PactContinuation n args) =
  text $ "(" <> ppName n <> " " <>
    (T.intercalate " " (map renderCompactText args)) <> ")"
