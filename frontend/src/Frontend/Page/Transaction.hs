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
    let status tout = case _toutResult tout of
                PactResult (Left _) -> elAttr "i" ("class" =: "red close icon" <> "title" =: "Succeeded") blank
                PactResult (Right _) -> elAttr "i" ("class" =: "green check icon" <> "title" =: "Failed") blank
    let requestkey tx = hashB64U $ _transaction_hash tx
    let _continuationCodeStep _tx = undefined
    let continuationInitRequestKey tx = case _pactCommand_payload $ _transaction_cmd tx of
          ExecPayload _ -> Nothing
          ContPayload c -> Just $ _cont_pactId c
    let mkTxDetailRoute n reqKey = mkNetRoute n (NetRoute_TxDetail :/ reqKey)
    el "h2" $ text $ (tshow $ length txs) <> " Transactions"
    elClass "table" "ui definition celled table" $ do
       el "thead" $ el "tr" $ do
        elAttr "th" ("style" =: "width: auto") $ text "Status"
        elAttr "th" ("style" =: "width:auto") $ text "Request Key"
        elAttr "th" ("style" =: "width:auto") $ text "Continuation (include init request key later)"
       el "tbody" $ do
        forM_ txs $ \(t, tout) -> do
          el "tr" $ do
            el "td" $ status tout
            el "td" $ routeLink (mkTxDetailRoute netId $ requestkey t) $ text $ requestkey t
            el "td" $ maybe (pure ()) text $ continuationInitRequestKey t

