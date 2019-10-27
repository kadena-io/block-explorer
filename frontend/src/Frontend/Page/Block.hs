{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Frontend.Page.Block where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Route
import           Common.Utils
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Page.Transaction
------------------------------------------------------------------------------


blockPage
  :: (MonadApp r t m, Monad (Client m), MonadJSM (Performable m), HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> App (Int :. Text :. R BlockRoute) t m ()
blockPage netId = do
    args <- askRoute
    void $ networkView (blockWidget netId <$> args)

blockWidget
  :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> Int :. Text :. R BlockRoute
  -> m ()
blockWidget netId (cid :. hash :. r) = do
  as <- ask
  let h = _as_host as
      c = ChainId cid
  ebh <- getBlockHeader h c hash
  void $ networkHold (text "Block does not exist") (blockPageNoPayload netId h c r <$> fmapMaybe id ebh)

blockLink
  :: (MonadApp r t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> ChainId
  -> Hash
  -> m ()
blockLink netId chainId hash =
  routeLink (addNetRoute netId $ unChainId chainId :. hashB64U hash :. Block_Header :/ ()) $ text $ hashHex hash

blockPageNoPayload
  :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> ChainwebHost
  -> ChainId
  -> R BlockRoute
  -> (BlockHeader, Text)
  -> m ()
blockPageNoPayload netId h c r bh = do
  let choose ep = case ep of
        Left e -> text $ "Block payload query failed: " <> T.pack e
        Right payload -> case r of
          Block_Header :/ _ -> blockHeaderPage netId h c bh payload
          Block_Transactions :/ _ -> transactionPage payload
  pEvt <- getBlockPayload h c (_blockHeader_payloadHash $ fst bh)
  void $ networkHold (text "Retrieving payload...") (choose <$> pEvt)


blockHeaderPage
  :: (MonadApp r t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> ChainwebHost
  -> ChainId
  -> (BlockHeader, Text)
  -> BlockPayload
  -> m ()
blockHeaderPage netId _ c (bh, bhBinBase64) bp = do
    el "h2" $ text "Block Header"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime $ _blockHeader_creationTime bh
        tfield "Chain" $ text $ tshow $ _blockHeader_chainId bh
        tfield "Block Height" $ text $ tshow $ _blockHeader_height bh
        tfield "Parent" $ parent $ _blockHeader_parent bh
        tfield "POW Hash" $ text $ either (const "") id (calcPowHash =<< decodeB64UrlNoPaddingText bhBinBase64)
        tfield "Target" $ text $ hexBytesLE $ _blockHeader_target bh
        tfield "Hash" $ text $ hashHex $ _blockHeader_hash bh
        tfield "Weight" $ text $ hexBytesLE $ _blockHeader_weight bh
        tfield "Epoch Start" $ text $ tshow $ posixSecondsToUTCTime $ _blockHeader_epochStart bh
        tfield "Neighbors" $ neighbors $ _blockHeader_neighbors bh
        tfield "Payload Hash" $ text $ hashB64U $ _blockHeader_payloadHash bh
        tfield "Chainweb Version" $ text $ _blockHeader_chainwebVer bh
        tfield "Nonce" $ text $ _blockHeader_nonce bh
        return ()
    blockPayloadWidget netId c bh bp
  where
    parent p = blockLink netId (_blockHeader_chainId bh) p
    neighbors ns = do
      forM_ (M.toList ns) $ \(cid,nh) -> do
        el "div" $ do
          text $ "Chain " <> tshow cid <> ": "
          blockLink netId cid nh

blockPayloadWidget
  :: (MonadApp r t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => NetId
  -> ChainId
  -> BlockHeader
  -> BlockPayload
  -> m ()
blockPayloadWidget netId c bh bp = do
    el "h2" $ text "Block Payload"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfield "Miner" $ do
          el "div" $ text $ "Account: " <> _minerData_account (_blockPayload_minerData bp)
          el "div" $ text $ "Public Keys: " <> tshow (_minerData_publicKeys $ _blockPayload_minerData bp)
          el "div" $ text $ "Predicate: " <> _minerData_predicate (_blockPayload_minerData bp)
        tfield "Transactions Hash" $ text $ hashB64U $ _blockPayload_transactionsHash bp
        tfield "Outputs Hash" $ text $ hashB64U $ _blockPayload_outputsHash bp
        tfield "Payload Hash" $ text $ hashB64U $ _blockPayload_payloadHash bp
        let rawHash = _blockHeader_hash bh
        let hash = hashB64U rawHash
        tfield "Transactions" $
          routeLink (addNetRoute netId $ unChainId c :. hash :. Block_Transactions :/ ()) $ text $ hashHex rawHash
