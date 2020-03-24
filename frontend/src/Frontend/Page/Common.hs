{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Page.Common
( unwrapJSON
, renderMetaData
, renderPayload
, transactionsLink
) where


------------------------------------------------------------------------------
import Control.Lens
------------------------------------------------------------------------------
import Data.Aeson as A
import Data.Foldable
import Data.Functor (void)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX

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
      elClass "table" "ui definition table" $ el "tbody" $ do
        tfield "Block Height" $ text $ tshow bh
        tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
        tfield "Block Hash" $ transactionsLink netId cid bhash
        tfield "Parent Hash" $ transactionsLink netId cid phash
    A.Error e -> text $ "Unable to decode metadata: " <> T.pack e

renderObject
    :: MonadApp r t m
    => Object
    -> m ()
renderObject m = void
    $ elClass "table" "ui definition table" $ el "tbody"
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
    ExecPayload (Exec c d) -> do
      elClass "table" "ui definition table" $ el "tbody" $ do
        tfield "Code" $ text c
        maybe (pure ()) (tfield "Data" . renderObject) d
    ContPayload (Cont pid rb step d p) -> do
      elClass "table" "ui definition table" $ el "tbody" $ do
        tfield "Pact Id" $ text pid
        tfield "Rollback" $ text $ tshow rb
        tfield "Step" $ text $ tshow step
        tfield "Data" $ renderObject d
        tfield "Cont Proof" $ text p
