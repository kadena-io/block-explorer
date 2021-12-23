{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Frontend.Page.TxDetail where

import Control.Monad
import Control.Monad.Reader
import Data.Aeson as A
import Data.Maybe
import qualified Data.Text as T
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Value)
import Reflex.Network
import Servant.Reflex

import Pact.Types.Continuation (PactExec)

import Chainweb.Api.ChainId
import ChainwebData.Api
import ChainwebData.TxDetail

import Common.Types
import Common.Utils
import Common.Route
import Frontend.App
import Frontend.AppState
import Frontend.ChainwebApi
import Frontend.Common
import Frontend.Page.Block
import Frontend.Page.Common


txDetailWidget
    :: ( MonadApp r t m
       , MonadJSM (Performable m)
       , HasJSContext (Performable m)
       , Prerender js t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => NetId
    -> App T.Text t m ()
txDetailWidget netId = do
  (AppState _ _ mnc _) <- ask
  case mnc of
    Nothing -> text "Tx detail not available for this network"
    Just nc -> do
      reqKey <- askRoute
      pb <- getPostBuild
      res <- getTxDetail nc
          (QParamSome . RequestKey <$> reqKey)
          (leftmost [pb, () <$ updated reqKey])
      void
          $ networkHold (inlineLoader "Querying blockchain ...")
          $ fmap (either text (txDetailPage netId)) res



txDetailPage
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => NetId
  -> TxDetail
  -> m ()
txDetailPage netId TxDetail{..} = do
  el "h2" $ text $ "Transaction Detail"
  elClass "table" "ui definition table" $ do
    el "tbody" $ do
      tfield "Request Key" $ text _txDetail_requestKey
      tfield "Chain" $ text $ tshow $ _txDetail_chain
      tfield "Block" $
          blockHashLink netId (ChainId _txDetail_chain) _txDetail_blockHash $ _txDetail_blockHash
      tfield "Code" $ case _txDetail_continuation of
        Nothing -> el "pre" $ text $ fromMaybe "" _txDetail_code
        Just {} -> text "Continuation"
      tfield "Transaction Output" $ do
        elClass "table" "ui definition table" $ el "tbody" $ do
          tfield "Gas" $ text $ tshow $ _txDetail_gas
          tfield "Result" $ do
            if _txDetail_success then
              elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") blank
                else
              elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") blank
            text $ pactValueJSON _txDetail_result
          tfield "Logs" $ text _txDetail_logs
          tfield "Metadata" $ renderMetaData netId (ChainId _txDetail_chain) (Just _txDetail_metadata)
          tfield "Continuation" $ voidMaybe renderCont $ _txDetail_continuation
          tfield "Transaction ID" $ text $ tshow _txDetail_txid
      tfield "Events" $ elClass "table" "ui definition table" $ el "tbody" $
        forM_ _txDetail_events $ \ ev -> el "tr" $ do
          elClass "td" "two wide" $ text (_txEvent_name ev)
          elClass "td" "evtd" $ elClass "table" "evtable" $
            forM_ (_txEvent_params ev) $ \v ->
              elClass "tr" "evtable" $ elClass "td" "evtable" $ text $ pactValueJSON v


      tfieldPre "Data" $ text $ prettyJSON _txDetail_data
      tfield "Nonce" $ text $ _txDetail_nonce
      tfield "Meta" $ do
        elClass "table" "ui definition table" $ el "tbody" $ do
          tfield "Chain" $ text $ tshow $ _txDetail_chain
          tfield "Sender" $ text $ _txDetail_sender
          tfield "Gas Price" $ text $ tshow $ _txDetail_gasPrice
          tfield "Gas Limit" $ text $ tshow $ _txDetail_gasLimit
          tfield "TTL" $ text $ tshow $ _txDetail_ttl
          tfield "Creation Time" $ text $ tshow $ _txDetail_creationTime


      -- tfield "Signers" $ do
      --   forM_ (_pactCommand_signers cmd) $ \s -> do
      --     elClass "table" "ui definition table" $ el "tbody" $ do
      --       tfield "Public Key" $ text $ _signer_pubKey s
      --       tfield "Account" $ text $ fromMaybe "" $ _signer_addr s
      --       tfield "Scheme" $ text $ fromMaybe "" $ _signer_scheme s
      --       tfield "Signature Capabilites" $ do
      --         when (not $ null $ _signer_capList s) $ do
      --           elClass "table" "ui celled table" $ do
      --             el "thead" $ do
      --               el "tr" $ do
      --                 el "th" $ text "Name"
      --                 el "th" $ text "Arguments"
      --             forM_ (_signer_capList s) $ \c -> do
      --               el "tbody" $ do
      --                 elClass "tr" "top aligned" $ do
      --                   el "td" $ text $ _scName c
      --                   elClass "td" "top aligned"
      --                     $ sequence
      --                     $ fmap (el "div" . text) (unwrapJSON <$> _scArgs c) <|> empty
      -- tfield "Signatures" $ do
      --   forM_ (_transaction_sigs t) $ \s -> do
      --     el "div" $ text $ unSig s
  where

    renderCont v = case fromJSON v of
      Success (pe :: PactExec) -> renderPactExec pe
      A.Error e -> text $ T.pack $ "Unable to render continuation" <> e
