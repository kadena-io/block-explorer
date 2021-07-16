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

------------------------------------------------------------------------------
import Control.Monad
import Data.Aeson as A
import qualified Data.Text as T
import Pact.Types.Continuation (PactExec)
import Reflex.Dom.Core hiding (Value)
------------------------------------------------------------------------------
import Chainweb.Api.ChainId
import ChainwebData.TxDetail

import Common.Types
import Common.Utils
import Common.Route
import Frontend.App
import Frontend.Common
import Frontend.Page.Common

import Obelisk.Route
import Obelisk.Route.Frontend

------------------------------------------------------------------------------

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
    -- let txs = _blockPayloadWithOutputs_transactionsWithOutputs bp
    -- el "h2" $ text $ (tshow $ length txs) <> " Transactions"
    -- divClass "ui accordion" $ do
    --  forM_ txs $ \(t, tout) -> mdo
    --    open <- toggle False $ domEvent Click e
    --    let cmd = _transaction_cmd t
        --let addActive cls active =
        --      ("class" =: if active then ("active " <> cls) else cls)
        --(e,_) <- elDynAttr' "div" (addActive "title" <$> open) $ do
        --  elClass "i" "dropdown icon" blank
        --  elClass "pre" "custombreak" $ text $ payloadCode $ _pactCommand_payload cmd
        -- elDynAttr "div" (addActive "content" <$> open) $ do
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Request Key" $ text _txDetail_requestKey
              tfield "Transaction Output" $ do
                elClass "table" "ui definition table" $ el "tbody" $ do
                  tfield "Gas" $ text $ tshow $ _txDetail_gas
                  tfield "Result" $ text $ pactValueJSON _txDetail_result
                  tfield "Logs" $ text _txDetail_logs
                  tfield "Metadata" $ renderMetaData netId (ChainId _txDetail_chain) (Just _txDetail_metadata)
                  tfield "Continuation" $ voidMaybe renderCont $ _txDetail_continuation
                  tfield "Transaction ID" $ text $ tshow _txDetail_txid
              tfield "Events" $ elClass "table" "ui definition table" $ el "tbody" $
                forM_ _txDetail_events $ \ ev -> el "tr" $ do
                  elClass "td" "two wide" $ text (_txEvent_name ev)
                  -- el "td" $ el "pre" $ text $ prettyJSON ev
                  elClass "td" "evtd" $ elClass "table" "evtable" $
                    forM_ (_txEvent_params ev) $ \v ->
                      elClass "tr" "evtable" $ elClass "td" "evtable" $ text $ pactValueJSON v


              --tfield "Payload" $ renderPayload _txDetail_data
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
