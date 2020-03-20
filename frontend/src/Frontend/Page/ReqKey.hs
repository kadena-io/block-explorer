{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.ReqKey where

------------------------------------------------------------------------------

import Control.Monad (join)
import Control.Monad.Reader

import Data.Aeson as A
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Time.Clock.POSIX
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack)
import Data.Word

import GHC.Generics (Generic)

import GHCJS.DOM.Types (MonadJSM)

import Obelisk.Route
import Obelisk.Route.Frontend

import Pact.Types.API
import qualified Pact.Types.Hash as Pact
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.PactError
import Pact.Types.Info

import Reflex.Dom.Core hiding (Value)
import Reflex.Network

import Text.Printf (printf)

------------------------------------------------------------------------------

import Chainweb.Api.ChainId
import Chainweb.Api.Common
import Chainweb.Api.Hash

import Common.Route
import Common.Types
import Common.Utils

import Frontend.App
import Frontend.AppState
import Frontend.Page.Block (blockLink)
import Frontend.ChainwebApi
import Frontend.Common
import Frontend.Page.Common
------------------------------------------------------------------------------

-- | The shape of enriched polling metadata from chainweb
--
data PollMetaData = PollMetaData
  { _pmd_BlockHeight :: BlockHeight
  , _pmd_CreationTime :: POSIXTime
  , _pmd_BlockHash :: Hash
  , _pmd_PrevBlockHash :: Hash
  } deriving (Eq, Show, Generic)

instance FromJSON PollMetaData where
  parseJSON = withObject "PollMetaData" $ \o -> PollMetaData
    <$> o .: "blockHeight"
    <*> fmap (/ 1000000.0) (o .: "blockTime")
    <*> o .: "blockHash"
    <*> o .: "prevBlockHash"


requestKeyWidget
    :: ( MonadApp r t m
       , MonadJSM (Performable m)
       , HasJSContext (Performable m)
       , Prerender js t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Monad (Client m)
       )
    => ServerInfo
    -> NetId
    -> Int
    -> App Text t m ()
requestKeyWidget si netId cid = do
    as <- ask
    let n = _as_network as
        chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
        c = ChainId cid

    reqKey <- askRoute
    cmdResult <- fromRequestKey chainwebHost c reqKey
    void $ networkHold (inlineLoader "Retrieving command result...") $ ffor cmdResult $ \case
      Nothing -> dynText (nothingMessage <$> reqKey)
      Just v@Object{} -> case fromJSON v of
        Success (PollResponses m)
          | HM.null m -> dynText (reqKeyMessage <$> reqKey)
          | otherwise -> traverse_ (requestKeyResultPage netId c) m
        A.Error e -> text (aesonMessage e)
      _ -> text $ nothingMessage "Poll fetch failed with wrong type"
  where
    nothingMessage s =
      T.pack $ "Unknown error returned while polling for request key: " <> (show s)
    aesonMessage s =
      T.pack $ "Unexpected result returned while polling for request key: " <> show s
    reqKeyMessage s =
      T.pack $ printf "Your request key %s is not associated with an already processed transaction." (show s)


requestKeyResultPage
    :: ( MonadApp r t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Prerender js t m
       )
    => NetId
    -> ChainId
    -> CommandResult Pact.Hash
    -> m ()
requestKeyResultPage netId cid (CommandResult rk txid pr g logs pcont meta) = do
    el "h2" $ text "Transaction"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfield "Request Key" $ text $ requestKeyToB16Text rk
        tfield "Transaction Id" $ text $ maybe "" tshow txid
        tfield "Result" $ renderPactResult pr
        tfield "Gas" $ text $ tshow g
        tfield "Logs" $ text $ maybe "" Pact.hashToText logs
        tfield "Continuation" $ text $ maybe "" tshow pcont
        tfield "Metadata" $ renderMeta meta
  where
    renderMeta Nothing = text ""
    renderMeta (Just v) = case fromJSON v of
      Success (PollMetaData bh bt bhash phash) -> el "div" $ do
        tfield "Block Height" $ text $ tshow bh
        tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
        tfield "Block Hash" $ link bh bhash
        tfield "Parent Hash" $ link (bh - 1) phash
      A.Error e -> text $ "Unable to decode metadata: " <> T.pack e

    renderPactResult (PactResult pr) =
      text $ join either unwrapJSON (bimap toJSON toJSON pr)

    link bh bhash = blockLink netId cid bh (hashHex bhash)
