{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.ReqKey
( requestKeyWidget
) where

------------------------------------------------------------------------------

import Control.Monad (join)
import Control.Monad.Reader

import Data.Aeson as A
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack)

import GHCJS.DOM.Types (MonadJSM)

import Obelisk.Route
import Obelisk.Route.Frontend

import Pact.Types.API
import qualified Pact.Types.Hash as Pact
import Pact.Types.Command

import Reflex.Dom.Core hiding (Value)
import Reflex.Network

import Text.Printf (printf)

------------------------------------------------------------------------------

import Chainweb.Api.ChainId

import Common.Route
import Common.Types
import Common.Utils

import Frontend.App
import Frontend.AppState
import Frontend.ChainwebApi
import Frontend.Common
import Frontend.Page.Common
------------------------------------------------------------------------------


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
      "Unknown error returned while polling for request key: " <> tshow s
    aesonMessage s =
      "Unexpected result returned while polling for request key: " <> tshow s
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
    el "h2" $ text "Transaction Results"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfield "Request Key" $ text $ requestKeyToB16Text rk
        tfield "Transaction Id" $ text $ maybe "" tshow txid
        tfield "Result" $ renderPactResult pr
        tfield "Gas" $ text $ tshow g
        tfield "Logs" $ text $ maybe "" Pact.hashToText logs
        tfield "Continuation" $ text $ maybe "" tshow pcont
        tfield "Metadata" $ renderMetaData netId cid meta
  where
    renderPactResult (PactResult r) =
      text $ join either unwrapJSON (bimap toJSON toJSON r)
