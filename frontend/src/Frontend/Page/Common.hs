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
, renderMiner
  -- * links
, transactionsLink
, blockHashLink
, blockHeightLink
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
import Chainweb.Api.Common
import Chainweb.Api.Hash
import Chainweb.Api.MinerData
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

-- | Link to a block hash directly, on a given chain
--
blockHashLink
    :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
    => NetId
    -> ChainId
    -> Hash
    -> m ()
blockHashLink netId c h =
    routeLink route $ text $ hashHex h
  where
    route = addNetRoute netId (unChainId c)
      $ Chain_BlockHash :/ (hashB64U h) :. Block_Header :/ ()

-- | Link to block associated with a given block height on a particular
-- chain
--
blockHeightLink
    :: ( MonadApp r t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Prerender js t m
       )
    => NetId
    -> ChainId
    -> BlockHeight
    -> Text
    -> m ()
blockHeightLink netId c height linkText =
    routeLink route $ text linkText
  where
    route = addNetRoute netId (unChainId c)
      $ Chain_BlockHeight :/ height :. Block_Header :/ ()

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
    -> PollMetaData
    -> m ()
renderMetaData netId cid (PollMetaData bh bt bhash phash) =
    singleLineTableSection $ do
      tfield "Block Height" $ text $ tshow bh
      tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
      tfield "Block Hash" $ blockHashLink netId cid bhash
      tfield "Parent Hash" $ blockHashLink netId cid phash


-- | Render an object as a structured table, instead of raw json
--
renderRichObject
    :: MonadApp r t m
    => Object
    -> m ()
renderRichObject m
    | HM.null m = pure ()
    | otherwise = void
      $ singleLineTableSection
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
      singleLineTableSection $ do
        voidMaybe (tfield "Data" . renderRichObject) d
    ContPayload (Cont pid rb step d p) -> do
      singleLineTableSection $ do
        tfield "Pact Id" $ text pid
        tfield "Rollback" $ text $ tshow rb
        tfield "Step" $ text $ tshow step
        tfield "Data" $ renderRichObject d
        tfield "Cont Proof" $ text p

-- | Render a 'PactExec' object in a 'CommandResult'
--
renderPactExec
    :: MonadApp r t m
    => PactExec
    -> m ()
renderPactExec (PactExec stepCount y x step (PactId pid) pcont rb) =
    singleLineTableSection $ do
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
    singleLineTableSection $ do
      tfield "Target Chain" $ text c
      tfield "Module Hash" $ text $ tshow mhash

-- | Render 'Yield' pact type
--
renderYield
    :: MonadApp r t m
    => Yield
    -> m ()
renderYield (Yield (ObjectMap m) p) =
    singleLineTableSection $ do
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
    singleLineTableSection $ do
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

renderMiner
    :: MonadApp r t m
    => MinerData
    -> m ()
renderMiner (MinerData acct p pkeys) =
    uiTableSection $ do
      tfield "Account" $ text $ tshow acct
      tfield "Public Keys" $ text $ tshow pkeys
      tfield "Predicate" $ text $ tshow $ p
