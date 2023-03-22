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
import Control.Applicative ((<|>))
import Control.Monad
import Data.Bool (bool)
import Reflex.Dom.Core hiding (Value)
------------------------------------------------------------------------------
import Chainweb.Api.BlockPayloadWithOutputs
import Chainweb.Api.ChainId
import Chainweb.Api.Hash
import Chainweb.Api.PactCommand
import Chainweb.Api.Payload
import Chainweb.Api.Transaction
import Common.Types
import Common.Utils
import Common.Route
import Frontend.App

import Obelisk.Route
import Obelisk.Route.Frontend

------------------------------------------------------------------------------

transactionPage
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => NetId
  -> ChainId
  -> BlockPayloadWithOutputs
  -> m ()
transactionPage netId _cid bp = do
    let txs = _blockPayloadWithOutputs_transactionsWithOutputs bp
    let isSuccess tout = case _toutResult tout of
          PactResult (Left _) -> False
          PactResult (Right _) -> True
    let status tout = if isSuccess tout then elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") blank
                else elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") blank
    let requestkey tx = hashB64U $ _transaction_hash tx
    let continuationCodeStep _tx = Nothing
    let continuationInitRequestKey tx = case _pactCommand_payload $ _transaction_cmd tx of
          ExecPayload _ -> Nothing
          ContPayload c -> Just $ _cont_pactId c
    let execCode tx = case _pactCommand_payload $ _transaction_cmd tx of
          ExecPayload e -> Just $ _exec_code e
          ContPayload _ -> Nothing
    let mkTxDetailRoute n reqKey = mkNetRoute n (NetRoute_TxDetail :/ reqKey)
    el "h2" $ text $ (tshow $ length txs) <> " Transactions"
    elClass "table" "ui compact celled table" $ do
       el "thead" $ el "tr" $ do
        elAttr "th" ("style" =: "width:auto") $ text "Status"
        elAttr "th" ("style" =: "width:auto") $ text "Request Key"
        elAttr "th" ("style" =: "width:auto") $ text "Continuation"
        elAttr "th" ("style" =: "width:auto") $ text "Code Preview"
       el "tbody" $ do
        forM_ txs $ \(t, tout) -> do
          el "tr" $ do
            elAttr "td" ("data-tooltip" =: bool "Failure" "Success" (isSuccess tout)) $ status tout
            elAttr "td" ("class" =: "cut-text" <> "style" =: "max-width: 200px") $ routeLink (mkTxDetailRoute netId $ requestkey t) $ text $ requestkey t
            elAttr "td" ("class" =: "cut-text" <> "style" =: "max-width: 200px") $ maybe (text "<No continuation>") text $ continuationInitRequestKey t
            -- TODO: add continuation code preview
            elAttr "td" ("class" =: "cut-text" <> "style" =: "max-width: 200px") $ maybe (text "no code to display") text $ (execCode t <|> continuationCodeStep t)

