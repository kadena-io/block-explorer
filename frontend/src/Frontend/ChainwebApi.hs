{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Frontend.ChainwebApi where

------------------------------------------------------------------------------
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Data.List
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           GHCJS.DOM.Types (MonadJSM)
import           Reflex.Dom hiding (Cut, Value)
------------------------------------------------------------------------------
import           Blake2Native
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BlockHeaderTx
import           Chainweb.Api.BlockPayload
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainTip
import           Chainweb.Api.Common
import           Chainweb.Api.Cut
import           Chainweb.Api.Hash
import           Chainweb.Api.RespItems
import           Common.Types
import           Common.Utils
------------------------------------------------------------------------------

apiBaseUrl :: ChainwebHost -> Text
apiBaseUrl (ChainwebHost h cver) =
    hostScheme h <> hostToText h <> "/chainweb/0.0/" <> cver <> "/"

cutUrl :: ChainwebHost -> Text
cutUrl h = apiBaseUrl h <> "cut"

chainBaseUrl :: ChainwebHost -> ChainId -> Text
chainBaseUrl h chainId = apiBaseUrl h <> "chain/" <> (tshow (unChainId chainId))

chainHashesUrl :: ChainwebHost -> BlockHeight -> BlockHeight -> ChainId -> Text
chainHashesUrl h minHeight maxHeight chainId = chainBaseUrl h chainId <>
  "/hash?minheight=" <> tshow minHeight <> "&maxheight=" <> tshow maxHeight

headersUrl :: ChainwebHost -> BlockHeight -> BlockHeight -> ChainId -> Text
headersUrl h minHeight maxHeight chainId = chainBaseUrl h chainId <>
  "/header?minheight=" <> tshow minHeight <> "&maxheight=" <> tshow maxHeight

headerUrl :: ChainwebHost -> ChainId -> Text -> Text
headerUrl h chainId blockHash = chainBaseUrl h chainId <> "/header/" <> blockHash

headerUpdatesUrl :: ChainwebHost -> Text
headerUpdatesUrl h = apiBaseUrl h <> "header/updates"

payloadUrl :: ChainwebHost -> ChainId -> Hash -> Text
payloadUrl h chainId payloadHash = chainBaseUrl h chainId <> "/payload/" <> hashB64U payloadHash

getServerInfo
  :: (PostBuild t m, TriggerEvent t m, PerformEvent t m,
      HasJSContext (Performable m), MonadJSM (Performable m), MonadHold t m)
  => Host
  -> m (Dynamic t (Maybe ServerInfo))
getServerInfo h = do
  pb <- getPostBuild
  esi <- getInfo (h <$ pb)
  holdDyn Nothing esi

getInfo
  :: forall t m. (TriggerEvent t m, PerformEvent t m, HasJSContext (Performable m), MonadJSM (Performable m))
  => Event t Host
  -> m (Event t (Maybe ServerInfo))
getInfo host = do
  let mkUrl h = hostScheme h <> hostToText h <> "/info"
  resp <- performRequestsAsync $ fmap (\h -> (h, XhrRequest "GET" (mkUrl h) def)) host
  return (decodeXhrResponse . snd <$> resp)

getCut
  :: (TriggerEvent t m, PerformEvent t m, HasJSContext (Performable m), MonadJSM (Performable m))
  => Event t ChainwebHost
  -> m (Event t (Maybe Cut))
getCut host = do
  resp <- performRequestsAsync $ fmap (\h -> (h, XhrRequest "GET" (cutUrl h) def)) host
  return (decodeXhrResponse . snd <$> resp)

getItems :: Value -> [Value]
getItems val = val ^.. key "items" . values

getBlockTable
  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => ChainwebHost
  -> CServerInfo
  -> m (Event t BlockTable)
getBlockTable h csi = do
  pb <- getPostBuild
  resp <- performRequestsAsync $ mkHeaderRequest HeaderJson h csi <$ pb
  return (foldl' (\bt val -> combineBlockTables bt val) mempty <$> (fmap decodeXhrResponse <$> resp))

getBlockHeader
  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => ChainwebHost
  -> ChainId
  -> Text
  -> m (Event t (Maybe (BlockHeader, Text)))
  -- ^ Returns the block header and the base64url-encoded binary serialization
getBlockHeader h c blockHash = do
  pb <- getPostBuild
  let reqs = [ mkSingleHeaderRequest HeaderJson h c blockHash
             , mkSingleHeaderRequest HeaderBinary h c blockHash
             -- NOTE: Order of this list must match the order of the argument to decodeResults
             ]
  resp <- performRequestsAsync $ reqs <$ pb
  let eRes = decodeResults <$> resp
  return (hush <$> eRes)

getBlockHeaderByHeight
  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => ChainwebHost
  -> ChainId
  -> BlockHeight
  -> m (Event t (Maybe (BlockHeader, Text)))
  -- ^ Returns the block header and the base64url-encoded binary serialization
getBlockHeaderByHeight h c blockHeight = do
  pb <- getPostBuild
  emcut <- getCut (h <$ pb)
  let cutHash = fmap _tipHash . HM.lookup c . _cutChains <$> fmapMaybe id emcut
  let mkReqs ch = [ mkAncestorHeaderRequest HeaderJson h c ch blockHeight blockHeight
                  , mkAncestorHeaderRequest HeaderBinary h c ch blockHeight blockHeight
                  -- NOTE: Order of this list must match the order of the argument to decodeResults
                  ]
  resp <- performRequestsAsync $ mkReqs <$> fmapMaybe id cutHash
  let eRes = decodeHeightResults <$> resp
  return (hush <$> traceEvent "eRes" eRes)

--getBlockHeaderByHeightOld
--  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
--  => ChainwebHost
--  -> ChainId
--  -> BlockHeight
--  -> m (Event t (Maybe (BlockHeader, Text)))
--  -- ^ Returns the block header and the base64url-encoded binary serialization
--getBlockHeaderByHeightOld h c blockHeight = do
--  pb <- getPostBuild
--  let reqs = [ mkSingleHeightRequest h c blockHeight
--             , mkSingleHeightRequestBinary h c blockHeight
--             -- NOTE: Order of this list must match the order of the argument to decodeResults
--             ]
--  resp <- performRequestsAsync $ reqs <$ pb
--  let eRes = decodeHeightResults <$> resp
--  return (hush <$> eRes)

decodeHeightResults :: [XhrResponse] -> Either String (BlockHeader, Text)
decodeHeightResults [bh, bhBin] = do
  h <- first ("Error decoding block header: " <>) $ decodeXhr bh
  hBin <- first ("Error decoding binary block header: " <>) $ decodeXhr bhBin
  return (head $ _respItems_items h, head $ _respItems_items hBin)
decodeHeightResults _ = Left "Invalid number of results"

decodeResults :: [XhrResponse] -> Either String (BlockHeader, Text)
decodeResults [bh, bhBin] = do
  h <- first ("Error decoding block header: " <>) $ decodeXhr bh
  hBin <- first ("Error decoding binary block header: " <>) $ decodeXhr bhBin
  return (h, hBin)
decodeResults _ = Left "Invalid number of results"

getBlockPayload
  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => ChainwebHost
  -> ChainId
  -> Hash
  -> m (Event t (Either String BlockPayload))
getBlockPayload h c payloadHash = do
    pb <- getPostBuild
    resp <- performRequestAsync $ req <$ pb
    return (decodeXhr <$> resp)
  where
    req = XhrRequest "GET" (payloadUrl h c payloadHash)
            (def { _xhrRequestConfig_headers = "accept" =: "application/json" })

decodeXhr :: FromJSON a => XhrResponse -> Either String a
decodeXhr = eitherDecode . BL.fromStrict . T.encodeUtf8 <=<
            note "decodeXhr encoding error" . _xhrResponse_responseText

combineBlockTables :: BlockTable -> Maybe Value -> BlockTable
combineBlockTables bt Nothing = bt
combineBlockTables bt0 (Just v) = foldl' (\bt b -> insertBlockTable bt (BlockHeaderTx b Nothing Nothing Nothing Nothing)) bt0 $ rights $
  map (parseEither parseJSON) $ getItems v

data HeaderEncoding = HeaderBinary | HeaderJson
  deriving (Eq,Ord,Show,Read,Enum)

headerEncoding :: HeaderEncoding -> Map Text Text
headerEncoding HeaderBinary = "accept" =: "application/json"
headerEncoding HeaderJson = "accept" =: "application/json;blockheader-encoding=object"

mkHeaderRequest :: HeaderEncoding -> ChainwebHost -> CServerInfo -> [XhrRequest ()]
mkHeaderRequest he h csi = map (\c -> (XhrRequest "GET" (headersUrl h minh maxh c) cfg))
                         $ siChainsList $ _csiServerInfo csi
  where
    cfg = def { _xhrRequestConfig_headers = headerEncoding he }
    maxh = _csiNewestBlockHeight csi
    minh = maxh - blockTableNumRows

mkSingleHeightRequest :: HeaderEncoding -> ChainwebHost -> ChainId -> BlockHeight -> XhrRequest ()
mkSingleHeightRequest he h c blockHeight = XhrRequest "GET" (headersUrl h blockHeight blockHeight c) cfg
  where
    cfg = def { _xhrRequestConfig_headers = headerEncoding he }

mkAncestorHeaderRequest
  :: HeaderEncoding
  -> ChainwebHost
  -> ChainId
  -> Text
  -> Int
  -> Int
  -> XhrRequest ByteString
mkAncestorHeaderRequest he h c cutHash minHeight maxHeight = XhrRequest "POST" url cfg
  where
    cfg = def { _xhrRequestConfig_headers = headerEncoding he <>
                                            "content-type" =: "application/json"
              , _xhrRequestConfig_sendData = body }
    body = BL.toStrict $ encode $ object [ "upper" .= [cutHash], "lower" .= ([] :: [Text]) ]
    url = chainBaseUrl h c <> "/header/branch?minheight=" <> tshow minHeight <>
          "&maxheight=" <> tshow maxHeight


mkSingleHeaderRequest :: HeaderEncoding -> ChainwebHost -> ChainId -> Text -> XhrRequest ()
mkSingleHeaderRequest he h c blockHash = XhrRequest "GET" (headerUrl h c blockHash) cfg
  where
    cfg = def { _xhrRequestConfig_headers = headerEncoding he }

calcPowHash :: ByteString -> Either String Text
calcPowHash bs = do
  h <- blake2s 32 "" $ B.take (B.length bs - 32) bs
  return $ T.decodeUtf8 $ B16.encode $ B.reverse h

addPayloadToTable
  :: BlockHeight
  -> ChainId
  -> BlockPayload
  -> BlockTable
  -> BlockTable
addPayloadToTable h c p bt = modifyBlockInTable bt h c f
  where
    f bhtx = bhtx { _blockHeaderTx_payload = Just p }

modifyBlockInTable
  :: BlockTable
  -> BlockHeight
  -> ChainId
  -> (BlockHeaderTx -> BlockHeaderTx)
  -> BlockTable
modifyBlockInTable (BlockTable bs cut) h c func = BlockTable bs2 cut2
  where
    bs2 = M.adjust (M.adjust func c) h bs
    cut2 = M.adjust g c cut
    height = _blockHeader_height . _blockHeaderTx_header
    g b = if height b == h then func b else b

data BlockTable = BlockTable
  { _blockTable_blocks :: Map BlockHeight (Map ChainId BlockHeaderTx)
  , _blockTable_cut    :: Map ChainId BlockHeaderTx
  } deriving (Eq)

instance Show BlockTable where
  show (BlockTable bs _) = unlines $ map show $ M.keys bs

instance Semigroup BlockTable where
  (BlockTable b1 c1) <> (BlockTable b2 c2) =
      BlockTable (M.unionWith M.union b1 b2) (M.unionWith f c1 c2)
    where
      height = _blockHeader_height . _blockHeaderTx_header
      f bh1 bh2 = if height bh1 > height bh2 then bh1 else bh2

instance Monoid BlockTable where
  mempty = BlockTable mempty mempty

blockTableNumRows :: Int
blockTableNumRows = 6

insertBlockTable :: BlockTable -> BlockHeaderTx -> BlockTable
insertBlockTable (BlockTable bs cut) btx = BlockTable bs2 cut2
  where
    b = _blockHeaderTx_header btx
    h = _blockHeader_height b
    c = _blockHeader_chainId b
    bs2 = M.delete (h - blockTableNumRows) $ M.alter f h bs
    cut2 = M.insertWith g c btx cut
    height = _blockHeader_height . _blockHeaderTx_header
    g b1 b2 = if height b1 > height b2 then b1 else b2

    f Nothing  = Just $ M.singleton c btx
    f (Just m) = Just $ M.insert c btx m

getBlock :: BlockHeight -> ChainId -> BlockTable -> Maybe BlockHeaderTx
getBlock bh cid bt = M.lookup cid =<< M.lookup bh (_blockTable_blocks bt)
