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
import Control.Applicative
import Control.Monad
import Data.Maybe
import Reflex.Dom.Core hiding (Value)
------------------------------------------------------------------------------
-- import Chainweb.Api.BlockPayload
import Chainweb.Api.BlockPayloadWithOutputs
import Chainweb.Api.ChainwebMeta
import Chainweb.Api.Hash
import Chainweb.Api.PactCommand
import Chainweb.Api.Payload
import Chainweb.Api.Sig
import Chainweb.Api.Signer
import Chainweb.Api.Transaction
import Common.Utils
import Frontend.App
import Frontend.Common
import Frontend.Page.Common
------------------------------------------------------------------------------

transactionPage
  :: (MonadApp r t m)
  => BlockPayloadWithOutputs
  -> m ()
transactionPage bp = do
    let txs = _blockPayloadWithOutputs_transactionsWithOutputs bp
    el "h2" $ text $ (tshow $ length txs) <> " Transactions"
    divClass "ui accordion" $ do
      forM_ txs $ \(t, tout) -> mdo
        open <- toggle False $ domEvent Click e
        let addActive cls active =
              ("class" =: if active then ("active " <> cls) else cls)
        (e,_) <- elDynAttr' "div" (addActive "title" <$> open) $ do
          elClass "i" "dropdown icon" blank
          -- elAttr "pre" ("style" =: "word-break: break-all; word-wrap: break-word;")  $ text $ payloadCode $ _pactCommand_payload $ _transaction_cmd t
          elClass "div" "custombreak" $ el "pre" $ text $ payloadCode $ _pactCommand_payload $ _transaction_cmd t
        elDynAttr "div" (addActive "content" <$> open) $ do
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Request Key" $ do
                let reqKey = hashB64U $ _transaction_hash t
                text reqKey
              tfield "Meta" $ do
                let meta = _pactCommand_meta $ _transaction_cmd t
                elClass "table" "ui definition table" $ el "tbody" $ do
                  tfield "Chain" $ text $ _chainwebMeta_chainId meta
                  tfield "Sender" $ text $ _chainwebMeta_sender meta
                  tfield "Gas Price" $ text $ tshow $ _chainwebMeta_gasPrice meta
                  tfield "Gas Limit" $ text $ tshow $ _chainwebMeta_gasLimit meta
                  tfield "TTL" $ text $ tshow $ _chainwebMeta_ttl meta
                  tfield "Creation Time" $ text $ tshow $ _chainwebMeta_creationTime meta
              tfield "Signers" $ do
                forM_ (_pactCommand_signers $ _transaction_cmd t) $ \s -> do
                  elClass "table" "ui definition table" $ el "tbody" $ do
                    tfield "Public Key" $ text $ _signer_pubKey s
                    tfield "Account" $ text $ fromMaybe "" $ _signer_addr s
                    tfield "Scheme" $ text $ fromMaybe "" $ _signer_scheme s
                    tfield "Signature Capabilites" $ do
                      elClass "table" "ui celled table" $ do
                        el "thead" $ do
                          el "tr" $ do
                            el "th" $ text "Name"
                            el "th" $ text "Arguments"
                        forM_ (_signer_capList s) $ \c -> do
                          el "tbody" $ do
                            elClass "tr" "top aligned" $ do
                              el "td" $ text $ _scName c
                              elClass "td" "top aligned"
                                $ sequence
                                $ fmap (el "div" . text) (unwrapJSON <$> _scArgs c) <|> empty
              tfield "Signatures" $ do
                forM_ (_transaction_sigs t) $ \s -> do
                  el "div" $ text $ unSig s
              tfield "Transaction Output" $ do
                elClass "table" "ui definition table" $ el "tbody" $ do
                  tfield "Gas" $ text $ tshow $ _toutGas tout
                  tfield "Result" $ text $ join either unwrapJSON $ fromPactResult $ _toutResult tout
                  tfield "Logs" $ text $ maybe "null " hashB64U $ _toutLogs tout
                  tfield "Metadata" $ text $ maybe "" tshow $ _toutMetaData tout
                  maybe (pure ()) (tfield "Continutaion" . text . tshow)  $ _toutContinuation tout
                  tfield "Transaction Id" $ text $ tshow $  _toutTxId tout
  where
    fromPactResult (PactResult pr) = pr
