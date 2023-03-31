{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Frontend.Page.Transaction where

------------------------------------------------------------------------------
import Control.Monad
import Data.Functor ((<&>))
import Reflex.Dom.Core hiding (Value)
------------------------------------------------------------------------------
import Chainweb.Api.BlockPayloadWithOutputs
import Chainweb.Api.ChainId
import Chainweb.Api.Hash
import Chainweb.Api.PactCommand
import Chainweb.Api.Payload
import Chainweb.Api.Transaction
import ChainwebData.Api
import ChainwebData.TxDetail
import Common.Types
import Common.Utils
import Common.Route
import Frontend.App
import Frontend.ChainwebApi
import Frontend.Common
import Safe (lastMay)
import Servant.Common.Req

import Language.Javascript.JSaddle.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Network

------------------------------------------------------------------------------

transactionPage
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     )
  => NetId
  -> Maybe NetConfig
  -> ChainId
  -> BlockPayloadWithOutputs
  -> Hash -- blockhash
  -> m ()
transactionPage netId netConfig cid bp hash = do
    let txs = _blockPayloadWithOutputs_transactionsWithOutputs bp
    let isSuccess tout = case _toutResult tout of
          PactResult (Left _) -> False
          PactResult (Right _) -> True
    let status tout = if isSuccess tout then elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") blank
                    else elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") blank
    let requestkey tx = hashB64U $ _transaction_hash tx
    let onPayload f g tx = case _pactCommand_payload $ _transaction_cmd tx of
          ExecPayload e -> f e
          ContPayload c -> g c
    let contExists = onPayload (const False) (const True)
    let mkTxDetailRoute n reqKey = mkNetRoute n (NetRoute_TxDetail :/ reqKey)
    let blockLink = routeLink (addNetRoute netId (unChainId cid) $
          Chain_BlockHash :/ (hashB64U hash, Block_Header :/ ())) $ text $ hashB64U hash

    let cutText = elAttr "div" ("class" =: "cut-text")
    pb <- getPostBuild

    el "h2" $ text $ "Block Transactions"
    elClass "table" "ui definition table" $ do
      el "tbody" $ do
        tfield "Block Hash" blockLink 
    elClass "table" "ui compact celled table" $ do
      el "thead" $ el "tr" $ do
        elAttr "th" ("style" =: "width: auto") $ text "Status"
        elAttr "th" ("style" =: "width: auto") $ text "Request Key"
        elAttr "th" ("style" =: "width: auto") $ text "Code"
      el "tbody" $ do
        forM_ txs $ \(t,tout) -> el "tr" $ do
          let reqKey = requestkey t
          elAttr "td" ("class" =: "center aligned") $ status tout
          elAttr "td" ("data-tooltip" =: requestkey t <> "style" =: "max-width: 400px; padding: 0px;") $ 
              el "div" $ cutText $ 
                  routeLink (mkTxDetailRoute netId $ requestkey t) $ text $ requestkey t
          let chainEmoji = "\x1F517"
              contToolTip = "This is a continuation of another transaction. The code is the initial code of the transaction that created this continuation."
          elAttr "td" ("style" =: "max-width: 630px; padding: 0px;" <> if contExists t then "data-tooltip" =: contToolTip else mempty) $ cutText $ do
            case _pactCommand_payload $ _transaction_cmd t of
              ExecPayload e -> routeLink (mkTxDetailRoute netId reqKey) $ text $ _exec_code e
              ContPayload _c -> do
                elAttr "span" ("class" =: "bubble-tag") $ text $ chainEmoji <> " "
                case netConfig of
                  Nothing -> text "Continuation"
                  Just nc -> do
                    res <- getTxDetails nc (constDyn $ QParamSome $ RequestKey reqKey) pb
                    void $ networkHold (el "it" $ text "Loading...") $ res <&> \case
                        Left err -> text $ tshow $ "Error: " <> err
                        Right rs -> case rs of
                          [] -> text "Continuation missing in the supporting database."
                          (r:_) -> case (,) <$> _txDetail_initialCode r <*> (lastMay =<< _txDetail_previousSteps r) of
                            Nothing -> text "Continuation missing in the supporting database."
                            Just (i,initReqKey) -> routeLink (mkTxDetailRoute netId initReqKey) $ text i
