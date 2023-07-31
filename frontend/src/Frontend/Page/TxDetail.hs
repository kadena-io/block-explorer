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
import Control.Lens (iforM_)
import Data.Aeson as A
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Value)
import Reflex.Network
import Servant.Reflex

import Pact.Types.Continuation (PactExec(..))

import Chainweb.Api.BlockHeader
import Chainweb.Api.ChainId
import Chainweb.Api.Hash
import Chainweb.Api.Signer
import Chainweb.Api.Sig
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
  (AppState _ si mnc _) <- ask
  case mnc of
    Nothing -> text "Tx detail not available for this network"
    Just nc -> do
      reqKey <- askRoute
      pb <- getPostBuild
      res <- getTxDetails nc
          (QParamSome . RequestKey <$> reqKey)
          (leftmost [pb, () <$ updated reqKey])
      void $ networkHold (inlineLoader "Querying blockchain ...") $ res <&> \case
        Left err -> text $ "Error fetching transactions: " <> err
        Right txDetails -> case NE.nonEmpty txDetails of
          Nothing -> text "No transaction found"
          Just neTxDetails -> txDetailPage nc netId (_siChainwebVer si) neTxDetails

txDetailPage
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , HasJSContext (Performable m)
     , MonadJSM (Performable m)
     , Prerender js t m
     )
  => NetConfig
  -> NetId
  -> ChainwebVersion
  -> NE.NonEmpty TxDetail
  -> m ()
txDetailPage nc netId cwVer txDetails@(firstDetail NE.:| restDetail) = do
  el "h2" $ text $ "Transaction Detail"
  elClass "table" "ui definition table" $ do
    el "tbody" $ do
      tfield "Request Key" $ text (_txDetail_requestKey $ firstDetail)
      tfield "Chain" $ text $ tshow $ (_txDetail_chain $ firstDetail)
      tfield "Block" $ do
        let tagIfOrphan cid height hash = if null restDetail
              then dynText $ constDyn mempty
              else do
                  let h = ChainwebHost (netHost netId) cwVer
                  winningHash <- _blockHeader_hash . fst <$$$> getBlockHeaderByHeight h cid height
                  t <- holdDyn " (Determining if an orphan...)" $
                        fmap (\whash -> if whash == hash then "" else " (orphan)") (fforMaybe winningHash (fmap hashB64U))
                  dynText t
        forM_ txDetails $ \tx -> el "tr" $ do
            blockHashLink netId (ChainId (_txDetail_chain tx)) (_txDetail_blockHash tx) $ (_txDetail_blockHash tx)
            tagIfOrphan (ChainId $ _txDetail_chain tx) (_txDetail_height tx) (_txDetail_blockHash tx)
      tfield "Code" $ case _txDetail_code firstDetail of
        Just c -> elAttr "pre" ("style" =: "white-space: pre-wrap;") $ text c
        Nothing -> do
          let previousSteps = _txDetail_previousSteps firstDetail
              initialCode = _txDetail_initialCode firstDetail
              mkTxDetailRoute rk = mkNetRoute netId (NetRoute_TxDetail :/ rk)
              txDetailLink rk = do
                text "Continuation of "
                routeLink (mkTxDetailRoute rk) $ text rk
          elClass "table" "ui definition table" $ el "tbody" $ do
            case previousSteps of
              Just steps -> tfield "Past Steps" $ do
                let l = length steps
                iforM_ steps $ \i step -> do
                  txDetailLink step
                  unless (i >= l - 1) $ el "br" blank
              Nothing -> text "No previous steps?"
            forM_ initialCode $ \c -> tfield "Initial Code" $ elAttr "pre" ("style" =: "white-space: pre-wrap;") $ text c
      tfield "Transaction Output" $ do
        elClass "table" "ui definition table" $ el "tbody" $ do
          tfield "Gas" $ text $ tshow $ _txDetail_gas firstDetail
          tfield "Result" $ do
            if _txDetail_success firstDetail then
              elAttr "i" ("class" =: "green check icon" <> "title" =: "Succeeded") blank
                else
              elAttr "i" ("class" =: "red close icon" <> "title" =: "Failed") blank
            text $ pactValueJSON $ _txDetail_result firstDetail
          tfield "Logs" $ text $ _txDetail_logs firstDetail
          tfield "Metadata" $ renderMetaData netId (ChainId $ _txDetail_chain firstDetail) (Just $ _txDetail_metadata firstDetail)
          tfield "Continuation" $ do
            pb <- getPostBuild
            let cont = _txDetail_continuation firstDetail
            let ditchPartialResult = \case
                       Left t -> Left t
                       Right (False,_) -> Left "A partial response is impossible!"
                       Right (True,r) -> Right r
            forM_ cont $ \c -> do
              res <- searchTxs nc
                 (constDyn Nothing)
                 (constDyn Nothing)
                 (constDyn QNone)
                 (constDyn (QParamSome $ _txDetail_requestKey firstDetail))
                 (constDyn QNone) (constDyn QNone) pb
              widgetHold_ (inlineLoader "Querying continuation info...") (renderCont c . ditchPartialResult <$> res)
          tfield "Transaction ID" $ text $ tshow $ _txDetail_txid firstDetail
      tfield "Events" $ elClass "table" "ui definition table" $ el "tbody" $
        forM_ (_txDetail_events firstDetail) $ \ ev -> el "tr" $ do
          elClass "td" "two wide" $ text (_txEvent_name ev)
          elClass "td" "evtd" $ elClass "table" "evtable" $
            forM_ (_txEvent_params ev) $ \v ->
              elClass "tr" "evtable" $ elClass "td" "evtable" $ text $ pactValueJSON v


      tfieldPre "Data" $ text $ prettyJSON $ _txDetail_data firstDetail
      tfield "Nonce" $ text $ _txDetail_nonce firstDetail
      tfield "Meta" $ do
        elClass "table" "ui definition table" $ el "tbody" $ do
          tfield "Chain" $ text $ tshow $ _txDetail_chain firstDetail
          tfield "Sender" $ text $ _txDetail_sender firstDetail
          tfield "Gas Price" $ text $ tshow $ _txDetail_gasPrice firstDetail
          tfield "Gas Limit" $ text $ tshow $ _txDetail_gasLimit firstDetail
          tfield "TTL" $ text $ tshow $ _txDetail_ttl firstDetail
          tfield "Creation Time" $ text $ tshow $ _txDetail_creationTime firstDetail


      tfield "Signers" $ do
        forM_ (_txDetail_signers firstDetail) $ \s -> do
          elClass "table" "ui definition table" $ el "tbody" $ do
            tfield "Public Key" $ text $ _signer_pubKey s
            forM_ (_signer_addr s) $ tfield "Address" . text
            forM_ (_signer_scheme s) $ tfield "Scheme" . text
            tfield "Signature Capabilities" $ do
              when (not $ null $ _signer_capList s) $ do
                elClass "table" "ui celled table" $ do
                  el "thead" $ do
                    el "tr" $ do
                      el "th" $ text "Name"
                      el "th" $ text "Arguments"

                  el "tbody" $
                    forM_ (_signer_capList s) $ \c -> el "tr" $ do
                      el "td" $ text $ _scName c
                      elClass "td" "evtd" $ elClass "table" "evtable" $
                        forM_ (_scArgs c) $ \arg -> elClass "tr" "evtable" $
                          elClass "td" "evtable" $ text $ unwrapJSON arg
      tfield "Signatures" $ do
        forM_ (_txDetail_sigs firstDetail) $ \s -> do
          el "div" $ text $ unSig s
  where
    renderCont v res = case fromJSON v of
      Success (pe :: PactExec) -> renderPactExec pe netId res
      A.Error e -> text $ T.pack $ "Unable to render continuation" <> e
