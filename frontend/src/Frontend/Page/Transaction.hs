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
import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
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
import Servant.Common.Req

import Language.Javascript.JSaddle.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend

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
  -> NetConfig
  -> ChainId
  -> BlockPayloadWithOutputs
  -> m ()
transactionPage netId netConfig _cid bp = do
    let txs = _blockPayloadWithOutputs_transactionsWithOutputs bp
    let isSuccess tout = case _toutResult tout of
          PactResult (Left _) -> False
          PactResult (Right _) -> True
    let status tout = if isSuccess tout then elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") (text "Success")
                    else elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") (text "Failure")
    let requestkey tx = hashB64U $ _transaction_hash tx
    let onPayload f g tx = case _pactCommand_payload $ _transaction_cmd tx of
          ExecPayload e -> f e
          ContPayload c -> g c
    let contExists = onPayload (const False) (const True)
    let execCode = onPayload (Just . _exec_code) (const Nothing)
    let mkTxDetailRoute n reqKey = mkNetRoute n (NetRoute_TxDetail :/ reqKey)
    pb <- getPostBuild

    el "h2" $ text $ (tshow $ length txs) <> " Transactions"
    elClass "table" "ui compact celled table" $ do
      el "thead" $ el "tr" $ do
        elAttr "th" ("style" =: "width: auto") $ text "Request Key"
        elAttr "th" ("style" =: "width: auto") $ text "Status"
        elAttr "th" ("style" =: "width: auto") $ text "Code"
      el "tbody" $ do
        forM_ txs $ \(t,tout) -> el "tr" $ do
          continuationCodeStep <- case contExists t of
            True -> do
              res <- getTxDetails netConfig (constDyn $ QParamSome $ RequestKey $ requestkey t) pb
              let initialCode = fmap (fmap (head . fmap _txDetail_initialCode)) res -- there should only be one
              -- let previousSteps = (fmap (fmap (fmap _txDetail_previousSteps)) res)
              return initialCode
            False -> return never
          elAttr "td" (mempty) $ do
            elAttr "span" ("data-tooltip" =: requestkey t) $
              elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 200px;") $
                  routeLink (mkTxDetailRoute netId $ requestkey t) $ text $ requestkey t
          el "td" $ status tout
          let chainEmoji = "\x1F517"
              contToolTip = "This is a continuation of another transaction. The code is the initial code of the transaction that created this continuation."
          el "td" $ elAttr "span" (if contExists t then "data-tooltip" =: contToolTip else mempty) $
            elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 200px;") $
              (>>= dynText) $ holdDyn (fromMaybe "loading..." $ execCode t) $
                continuationCodeStep <&> \case
                  Left err -> tshow $ "Error: " <> err
                  Right Nothing -> "No continuation"
                  Right r -> fromMaybe "impossible" (r <&> (\rr -> chainEmoji <> " " <> rr))
