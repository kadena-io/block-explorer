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
import Control.Lens (iforM_)
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
    let status tout = do
                elAttr "b" mempty (text "Status: ")
                if isSuccess tout then elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") (text "Success")
                    else elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") (text "Failure")
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
    divClass "centered-container" $ iforM_ txs $ \i (t,tout) -> divClass "ui card" $ divClass "content" $ do
            divClass "header" $ text $ "Transaction " <> tshow (succ i)
            divClass "content" $ do
              elAttr "h4" ("class" =: "ui sub header") $ text "Transaction Details"
              divClass "ui small feed" $ do

                divClass "summary" $ elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 200px") $ status tout
                 
                divClass "summary" $
                  elAttr "span" ("data-tooltip" =: requestkey t) 
                    $ elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 200px") $ do
                        elAttr "b" mempty (text "Request Key: ")
                        routeLink (mkTxDetailRoute netId $ requestkey t) $ text $ requestkey t

                let contInfo = continuationInitRequestKey t
                divClass "summary" $ elAttr "span" (foldMap ("data-tooltip" =:) contInfo) $
                  elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 275px") $ do
                    elAttr "b" mempty (text "Init Cont. Request Key: ")
                    maybe (text "<No continuation>") text contInfo

                divClass "summary" $ elAttr "div" ("class" =: "cut-text" <> "style" =: "max-width: 300px") $ maybe (text "no code to display") text $ (execCode t <|> continuationCodeStep t)

