{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

-- {-# LANGUAGE ConstraintKinds            #-}
-- {-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE RecursiveDo                #-}
-- {-# LANGUAGE ScopedTypeVariables        #-}
-- {-# LANGUAGE TupleSections              #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE TypeOperators              #-}
module Frontend.Transactions where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Configs
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Dom.EventSource
import           Reflex.Network
import           Text.Printf
------------------------------------------------------------------------------
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BlockHeaderTx
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainTip
import           Chainweb.Api.Common
import           Chainweb.Api.Cut
import           Chainweb.Api.Hash
import           ChainwebData.TxSummary
import           Common.Route
import           Common.Types
import           Common.Utils
import           Frontend.About
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Nav
import           Frontend.Page.Block
import           Frontend.Page.ReqKey
------------------------------------------------------------------------------

recentTransactions
  :: (SetRoute t (R FrontendRoute) m,
      RouteToUrl (R FrontendRoute) m, MonadReader (AppState t1) m,
      HasJSContext (Performable m), MonadJSM (Performable m),
      DomBuilder t m, PerformEvent t m, TriggerEvent t m, PostBuild t m,
      MonadHold t m)
  => RecentTxs
  -> m ()
recentTransactions txs = do
  net <- asks _as_network
  pb <- getPostBuild
  txTable net $ getSummaries txs

txTable
  :: (DomBuilder t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> [TxSummary]
  -> m ()
txTable net txs = do
  elClass "table" "ui compact celled table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ text "Chain"
      el "th" $ text "Height"
      elClass "th" "two wide" $ text "Sender"
      el "th" $ text "Transaction Code"
    el "tbody" $ do
      forM_ (take 5 txs) $ \tx -> el "tr" $ do
        let chain = _txSummary_chain tx
        let height = _txSummary_height tx
        let route = addNetRoute net chain $ Chain_BlockHeight :/ height :. Block_Header :/ ()
        elAttr "td" ("data-label" =: "Chain") $ text $ tshow chain
        elAttr "td" ("data-label" =: "Height") $ routeLink route $ text $ tshow height
        elAttr "td" ("data-label" =: "Sender") $ senderWidget tx
        elAttr "td" ("data-label" =: "Code") $ text $ fromMaybe "" $ _txSummary_code tx

senderWidget :: DomBuilder t m => TxSummary -> m ()
senderWidget tx = text $
    if isPublicKey s
      then T.take 12 s <> "..."
      else if T.length s > 16
              then T.take 16 s <> "..."
              else s
  where
    s = _txSummary_sender tx
