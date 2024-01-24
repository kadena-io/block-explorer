{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.Page.ReqKey
( requestKeyWidget
) where

------------------------------------------------------------------------------

import Control.Monad (join)
import Control.Monad.Reader
import Control.Lens

import Data.Aeson as A
import Data.Aeson.Lens
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Compose (Compose(..))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.Encoding as T


import Chainweb.Api.PactPollResponses

import GHCJS.DOM.Types (MonadJSM)

import Obelisk.Route
import Obelisk.Route.Frontend

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
       )
    => ServerInfo
    -> NetId
    -> App Text t m ()
requestKeyWidget si netId = do
    as <- ask
    reqKey <- askRoute
    let chainwebHost = ChainwebHost (_as_netConfig as) (_siChainwebVer si)
        xhrs rk = M.fromList $ map (\c -> (c, requestKeyXhr chainwebHost c rk)) $
                      S.toList $ _siChains si

    pb <- getPostBuild
    results <- performRequestsAsync $ tag (current $ xhrs <$> reqKey) pb
    void $ networkHold (inlineLoader "Retrieving command result...") $
      ffor results $ \resmap -> do
        -- text $ tshow $ fmap showResp resmap
        case M.toList $ M.mapMaybe decodeAndDropEmpty resmap of
          [] -> dynText (reqKeyMessage <$> reqKey)
          rs -> do
            let go (_, Left e) = reqParseErrorMsg e reqKey
                go (c, Right v) = traverse_ (requestKeyResultPage netId c) v
            mapM_ go rs
  where
    decodeWithErr resp = do
      guard $ _xhrResponse_status resp == 200
      rt <- _xhrResponse_responseText resp
      pure $ first tshow $ eitherDecode @PollResponses $ BL.fromStrict $ T.encodeUtf8 rt

    decodeAndDropEmpty resp = do
      res <- decodeWithErr resp
      case res of
        Left e -> pure $ Left e
        Right (PollResponses pr) -> if HM.null pr
                                    then Nothing
                                    else pure $ Right pr
    reqParseErrorMsg e rk = do
      el "div" $ dynText $
        fmap ("An unexpected error occured when processing request key: " <>) rk
      el "div" $ text "Help us make our tools better by filing an issue with the below message at www.github.com/kadena-io/block-explorer :"
      el "div" $ text e

    reqKeyMessage s =
      T.pack $ printf "Your request key %s is not associated with an already processed transaction on any chain." (show s)


requestKeyResultPage
    :: ( MonadApp r t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Prerender js t m
       )
    => NetId
    -> ChainId
    -> CommandResult PactHash
    -> m ()
requestKeyResultPage netId cid (CommandResult (RequestKey rk) txid pr (Gas g) logs pcont meta evs) = do
    el "h2" $ text "Transaction Results"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfieldLeaf "Chain" $ text $ tshow $ unChainId cid
        tfieldLeaf "Request Key" $ text rk
        tfieldLeaf "Transaction Id" $ text $ maybe "" tshow txid
        tfieldLeaf "Result" $ renderPactResult pr
        tfieldLeaf "Gas" $ text $ tshow g
        tfieldLeaf "Logs" $ text $ maybe "" unHash logs
        tfieldLeaf "Continuation" $ text $ maybe "" tshow pcont
        tfield "Metadata" $ renderMetaData netId cid meta
        tfield "Events" $ elClass "table" "ui definition table" $ el "tbody" $
                forM_ evs $ \ ev -> tfieldLeaf (fromMaybe "" (getEventName ev)) $
                  elClass "table" "evtable" $
                    forM_ (Compose $ getEventParams ev) $ \v ->
                      elClass "tr" "evtable" $ elClass "td" "evtable" $
                        text $ unwrapJSON v
  where
    getEventName ev = go
        <$> ev ^? key "module" . key "name" . _String
        <*> ev ^? key "name" . _String
      where
       go moduleName eventName = moduleName <> "." <> eventName
    getEventParams ev = ev ^? key "params" . _Array
    renderPactResult (PactResult r) =
      text $ join either unwrapJSON (bimap toJSON toJSON r)
