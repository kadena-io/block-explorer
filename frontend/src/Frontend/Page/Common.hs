{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Common
( -- * useful combinators
  unwrapJSON
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
------------------------------------------------------------------------------
import Data.Aeson as A
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX

-- ------------------------------------------------------------------------ --
-- Pact modules

import qualified Pact.Types.ChainId as Pact
import Pact.Types.Continuation
import Pact.Types.Names
import Pact.Types.Term (FieldKey(..), ObjectMap(..), PactId(..))

-- ------------------------------------------------------------------------ --
-- Chainweb modules

import Chainweb.Api.ChainId
import Chainweb.Api.Hash
import Chainweb.Api.Payload

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
      $ Chain_BlockHash :/ (hashB64U bhash) :. Block_Transactions :/ ()

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
renderMetaData netId cid (Just v) = case fromJSON v of
    Success (PollMetaData bh bt bhash phash) -> do
      detailsSection $ do
        tfield "Block Height" $ text $ tshow bh
        tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
        tfield "Block Hash" $ transactionsLink netId cid bhash
        tfield "Parent Hash" $ transactionsLink netId cid phash
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
    go label v = tfield label $ text $ unwrapJSON v

-- | Render the 'Payload' of a 'Transaction'
--
renderPayload
    :: MonadApp r t m
    => Payload
    -> m ()
renderPayload = \case
    ExecPayload (Exec _ d) -> do
      detailsSection $ do
        voidMaybe (tfield "Data" . text . unwrapJSON) d
    ContPayload (Cont pid rb step d p) -> do
      detailsSection $ do
        tfield "Pact Id" $ text pid
        tfield "Rollback" $ text $ tshow rb
        tfield "Step" $ text $ tshow step
        tfield "Data" $ text $ unwrapJSON d
        tfield "Cont Proof" $ text p

-- | Render a 'PactExec' object in a 'CommandResult'
--
renderPactExec
    :: MonadApp r t m
    => PactExec
    -> m ()
renderPactExec (PactExec stepCount y x step (PactId pid) pcont rb) =
    detailsSection $ do
      tfield "Step Count" $ text $ tshow stepCount
      voidMaybe (tfield "Yield" . renderYield) y
      voidMaybe (tfield "Executed" . text . tshow) x
      tfield "Step" $ text $ tshow step
      tfield "Pact Id" $ text pid
      tfield "Metadata" $ renderContinuation pcont
      tfield "Rollback" $ text $ tshow rb

-- | Render the 'Provenance' pact type
--
renderProvenance
    :: MonadApp r t m
    => Provenance
    -> m ()
renderProvenance (Provenance (Pact.ChainId c) mhash) =
    detailsSection $ do
      tfield "Target Chain" $ text c
      tfield "Module Hash" $ text $ tshow mhash

-- | Render 'Yield' pact type
--
renderYield
    :: MonadApp r t m
    => Yield
    -> m ()
renderYield (Yield (ObjectMap m) p) =
    detailsSection $ do
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
    detailsSection $ do
      tfield "Name" $ text $ ppName n
      renderArgs args
  where
    renderArgs [] = pure ()
    renderArgs (a:as) = do
      tfield "Args" $ go a
      traverse_ (tfield "" . go) as

    go = text
      . unwrapJSON
      . toJSON
