{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
import           Servant.Reflex
import           Text.Printf
import           Text.Read
------------------------------------------------------------------------------
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BlockHeaderTx
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainTip
import           Chainweb.Api.Common
import           Chainweb.Api.Cut
import           Chainweb.Api.Hash
import           ChainwebData.Pagination
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
  txTable net $ take 5 $ getSummaries txs

qParam :: Text
qParam = "q"

limParam :: Text
limParam = "lim"

pageParam :: Text
pageParam = "page"

itemsPerPage :: Integer
itemsPerPage = 20

transactionSearch
    :: ( MonadApp r t m
       , MonadJSM (Performable m)
       , HasJSContext (Performable m)
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => ServerInfo
    -> NetId
    -> App (Map Text (Maybe Text)) t m ()
transactionSearch si netId = do
    (AppState n si mdbh) <- ask
    case mdbh of
      Nothing -> text "Transaction search feature not available for this network"
      Just dbh -> do
        pmap <- askRoute
        pb <- getPostBuild
        let page = do
              pm <- pmap
              pure $ fromMaybe 1 $ readMaybe . T.unpack =<< join (M.lookup pageParam pm)
            needle = do
              pm <- pmap
              pure $ fromMaybe "" $ join (M.lookup qParam pm)
            newSearch = leftmost [pb, () <$ updated pmap]
        res <- searchTxs dbh (constDyn $ QParamSome $ Limit itemsPerPage)
                             (QParamSome . Offset . (*itemsPerPage) <$> page)
                             (QParamSome <$> needle) newSearch

        divClass "ui pagination menu" $ do
          let setSearchRoute f e = setRoute $
                tag (current $ mkTxSearchRoute n <$> needle <*> fmap (Just . f) page) e
              prevAttrs p = if p == 1
                              then "class" =: "disabled item"
                              else "class" =: "item"
          (p,_) <- elDynAttr' "div" (prevAttrs <$> page) $ text "Prev"
          setSearchRoute pred (domEvent Click p)
          divClass "disabled item" $ display page
          (n,_) <- elAttr' "div" ("class" =: "item") $ text "Next"
          setSearchRoute succ (domEvent Click n)

        let f = either text (txTable n)
        void $ networkHold (inlineLoader "Querying blockchain...") (f <$> res)

mkTxSearchRoute :: NetId -> Text -> Maybe Integer -> R FrontendRoute
mkTxSearchRoute netId str page =
  case netId of
    NetId_Mainnet -> FR_Mainnet :/ NetRoute_TxSearch :/ (qParam =: Just str <> p )
    NetId_Testnet -> FR_Testnet :/ NetRoute_TxSearch :/ (qParam =: Just str <> p)
    NetId_Custom host -> FR_Customnet :/ (host :. (NetRoute_TxSearch :/ (qParam =: Just str <> p)))
  where
    p = maybe mempty ((pageParam =:) . Just . tshow) page


uiPagination :: DomBuilder t m => m ()
uiPagination = do
  divClass "ui pagination menu" $ do
    divClass "item" blank

txTable
  :: (DomBuilder t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> [TxSummary]
  -> m ()
txTable net [] = blank
txTable net txs = do
  elClass "table" "ui compact celled table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ text "Chain"
      el "th" $ text "Height"
      elClass "th" "two wide" $ text "Sender"
      el "th" $ text "Transaction Code"
    el "tbody" $ do
      forM_ txs $ \tx -> el "tr" $ do
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
