{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}

module Frontend.ChainwebApi where

------------------------------------------------------------------------------
import           Control.Applicative (liftA2)
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           GHCJS.DOM.Types (MonadJSM)
import           Reflex.Dom hiding (Cut, Value)
import           Text.Printf
------------------------------------------------------------------------------
import           Blake2Native
import           Common.Utils
import           Common.Types
import           Frontend.Common
------------------------------------------------------------------------------

apiBaseUrl :: ChainwebHost -> Text
apiBaseUrl (ChainwebHost h cver) =
    T.pack $ printf "https://%s/chainweb/0.0/%s/" (T.unpack $ hostToText h) (T.unpack $ versionText cver)

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
  -> m (Dynamic t (Maybe CServerInfo))
getServerInfo h = do
  pb <- getPostBuild
  esi <- getInfo (h <$ pb)
  si <- holdDyn Nothing esi
  let ch = ChainwebHost h . _siChainwebVer <$> fmapMaybe id esi
      f = maximum . map _tipHeight . HM.elems . _cutChains
  height <- holdDyn Nothing =<< f <$$$> getCut ch
  pure $ (liftA2 . liftA2) CServerInfo si height

getInfo
  :: forall t m. (TriggerEvent t m, PerformEvent t m, HasJSContext (Performable m), MonadJSM (Performable m))
  => Event t Host
  -> m (Event t (Maybe ServerInfo))
getInfo host = do
  let mkUrl h = "https://" <> hostToText h <> "/info"
  resp <- performRequestsAsync $ fmap (\h -> (h, XhrRequest "GET" (mkUrl h) def)) host
  return (decodeXhrResponse . snd <$> resp)

data ChainTip = ChainTip
  { _tipHeight :: Int
  , _tipHash :: Text
  } deriving (Eq,Ord,Show)

instance FromJSON ChainTip where
  parseJSON = withObject "ChainTip" $ \o -> ChainTip
    <$> o .: "height"
    <*> o .: "hash"

data Cut = Cut
  { _cutId :: Text
  , _cutHeight :: BlockHeight
  , _cutWeight :: Text
  , _cutInstance :: Text
  , _cutChains :: HashMap ChainId ChainTip
  } deriving (Eq,Ord,Show)

instance FromJSON Cut where
  parseJSON = withObject "Cut" $ \o -> Cut
    <$> o .: "id"
    <*> o .: "height"
    <*> o .: "weight"
    <*> o .: "instance"
    <*> o .: "hashes"
    -- <*> (HM.fromList . map (first fromText) . HM.toList <$> (o .: "hashes"))

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
  resp <- performRequestsAsync $ mkHeaderRequest h csi <$ pb
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
  let reqs = [ mkSingleHeaderRequest h c blockHash
             , mkSingleHeaderRequestBinary h c blockHash
             -- NOTE: Order of this list must match the order of the argument to decodeResults
             ]
  resp <- performRequestsAsync $ reqs <$ pb
  let eRes = decodeResults <$> resp
  return (hush <$> eRes)

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
combineBlockTables bt0 (Just v) = foldl' (\bt b -> insertBlockTable bt (BlockHeaderTx b Nothing Nothing Nothing)) bt0 $ rights $
  map (parseEither parseJSON) $ getItems v

mkHeaderRequest :: ChainwebHost -> CServerInfo -> [XhrRequest ()]
mkHeaderRequest h csi = map (\c -> (XhrRequest "GET" (headersUrl h minh maxh c) cfg))
                         $ _siChains $ _csiServerInfo csi
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }
    maxh = _csiNewestBlockHeight csi
    minh = maxh - blockTableNumRows

mkSingleHeaderRequest :: ChainwebHost -> ChainId -> Text -> XhrRequest ()
mkSingleHeaderRequest h c blockHash = XhrRequest "GET" (headerUrl h c blockHash) cfg
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }

mkSingleHeaderRequestBinary :: ChainwebHost -> ChainId -> Text -> XhrRequest ()
mkSingleHeaderRequestBinary h c blockHash = XhrRequest "GET" (headerUrl h c blockHash) cfg
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json" }

newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq,Ord,Show,Read)

instance FromJSON Hash where
  parseJSON (String t) =
    either (\e -> fail $ "Base64Url parse failed: " <> e) (return . Hash) $
      decodeB64UrlNoPaddingText t
  parseJSON invalid = typeMismatch "String" invalid

hashHex :: Hash -> Text
hashHex = T.decodeUtf8 . B16.encode . unHash

hashB64U :: Hash -> Text
hashB64U = T.decodeUtf8 . B64U.encode . unHash

calcPowHash :: ByteString -> Either String Text
calcPowHash bs = do
  h <- blake2s 32 "" $ B.take (B.length bs - 32) bs
  return $ T.decodeUtf8 $ B16.encode $ B.reverse h

data BlockHeaderTx = BlockHeaderTx
  { _blockHeaderTx_header :: BlockHeader
  , _blockHeaderTx_txCount :: Maybe Int
  , _blockHeaderTx_powHash :: Maybe Text
  , _blockHeaderTx_target :: Maybe Text
  } deriving (Eq,Ord,Show)

instance FromJSON BlockHeaderTx where
  parseJSON = withObject "BlockHeaderTx" $ \o -> BlockHeaderTx
    <$> o .: "header"
    <*> o .: "txCount"
    <*> o .:? "powHash"
    <*> o .:? "target"

newtype BytesLE = BytesLE
  { unBytesLE :: ByteString
  } deriving (Eq,Ord,Show)

hexBytesLE :: BytesLE -> Text
hexBytesLE = T.decodeUtf8 . B16.encode . unBytesLE

leToInteger :: ByteString -> Integer
leToInteger bs = B.foldl' (\a b -> a * 256 + fromIntegral b) 0 bs

instance FromJSON BytesLE where
  parseJSON = withText "BytesLE" $
    either fail (return . BytesLE . B.reverse) . decodeB64UrlNoPaddingText

data BlockHeader = BlockHeader
  { _blockHeader_creationTime :: POSIXTime
  , _blockHeader_parent :: Hash
  , _blockHeader_height :: BlockHeight
  , _blockHeader_hash :: Hash
  , _blockHeader_chainId :: ChainId
  , _blockHeader_weight :: BytesLE
  , _blockHeader_epochStart :: POSIXTime
  , _blockHeader_neighbors :: Map ChainId Hash
  , _blockHeader_payloadHash :: Hash
  , _blockHeader_chainwebVer :: Text
  , _blockHeader_target :: BytesLE
  , _blockHeader_nonce :: Text
  } deriving (Eq,Ord,Show)

blockDifficulty :: BlockHeader -> Double
blockDifficulty =
  fromIntegral . targetToDifficulty . leToInteger . unBytesLE . _blockHeader_target

targetToDifficulty :: Integer -> Integer
targetToDifficulty target = (2 ^ (256 :: Int) - 1) `div` target

instance FromJSON BlockHeader where
  parseJSON = withObject "BlockHeader" $ \o -> BlockHeader
    <$> (fmap (/1000000.0) $ o .: "creationTime")
    <*> o .: "parent"
    <*> o .: "height"
    <*> o .: "hash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> (fmap (/1000000.0) $ o .: "epochStart")
    <*> o .: "adjacents"
    <*> (o .: "payloadHash")
    <*> o .: "chainwebVersion"
    <*> o .: "target"
    <*> o .: "nonce"

data BlockTable = BlockTable
  { _blockTable_blocks :: Map BlockHeight (Map ChainId BlockHeaderTx)
  , _blockTable_cut :: Map ChainId BlockHeaderTx
  } deriving (Eq,Ord)

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

    f Nothing = Just $ M.singleton c btx
    f (Just m) = Just $ M.insert c btx m

getBlock :: BlockHeight -> ChainId -> BlockTable -> Maybe BlockHeaderTx
getBlock bh cid bt = M.lookup cid =<< M.lookup bh (_blockTable_blocks bt)

data MinerData = MinerData
  { _minerData_account :: Text
  , _minerData_predicate :: Text
  , _minerData_publicKeys :: [Text]
  } deriving (Eq,Ord,Show)

instance FromJSON MinerData where
  parseJSON = withObject "MinerData" $ \o -> MinerData
    <$> o .: "account"
    <*> o .: "predicate"
    <*> o .: "public-keys"

data Transaction = Transaction
  { _transaction_hash :: Hash
  , _transaction_sigs :: [Sig]
  , _transaction_cmd :: PactCommand
  } deriving (Eq,Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> Transaction
    <$> o .: "hash"
    <*> o .: "sigs"
    <*> (withEmbeddedJSON "sig-embedded" parseJSON =<< (o .: "cmd"))

newtype Sig = Sig { unSig :: Text }
  deriving (Eq,Show)

instance FromJSON Sig where
  parseJSON = withObject "Sig" $ \o -> Sig
    <$> o .: "sig"

data PactCommand = PactCommand
  { _pactCommand_payload :: Payload
  , _pactCommand_signers :: [Signer]
  , _pactCommand_meta :: ChainwebMeta
  , _pactCommand_nonce :: Text
  } deriving (Eq,Show)

instance FromJSON PactCommand where
  parseJSON = withObject "PactCommand" $ \o -> PactCommand
    <$> o .: "payload"
    <*> o .: "signers"
    <*> o .: "meta"
    <*> o .: "nonce"

data Payload = ExecPayload Exec | ContPayload Cont
  deriving (Eq,Show)

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o -> do
    case HM.lookup "exec" o of
      Nothing -> case HM.lookup "cont" o of
                   Nothing -> fail "Payload must be exec or cont"
                   Just v -> ContPayload <$> parseJSON v
      Just v -> ExecPayload <$> parseJSON v

payloadCode :: Payload -> Text
payloadCode (ExecPayload e) = _exec_code e
payloadCode (ContPayload c) = _cont_pactId c

data Exec = Exec
  { _exec_code :: Text
  , _exec_data :: Object
  } deriving (Eq,Show)

instance FromJSON Exec where
  parseJSON = withObject "Exec" $ \o -> Exec
    <$> o .: "code"
    <*> o .: "data"

data Cont = Cont
  { _cont_pactId :: Text
  , _cont_rollback :: Bool
  , _cont_step :: Int
  , _cont_data :: Object
  , _cont_proof :: Text
  } deriving (Eq,Show)

instance FromJSON Cont where
  parseJSON = withObject "Cont" $ \o -> Cont
    <$> o .: "pactId"
    <*> o .: "rollback"
    <*> o .: "step"
    <*> o .: "data"
    <*> o .: "proof"

data Signer = Signer
  { _signer_addr :: Maybe Text
  , _signer_scheme :: Maybe Text
  , _signer_pubKey :: Text
  } deriving (Eq,Ord,Show)

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "addr"
    <*> o .:? "scheme"
    <*> o .: "pubKey"

data ChainwebMeta = ChainwebMeta
  { _chainwebMeta_chainId :: Text
  , _chainwebMeta_creationTime :: POSIXTime
  , _chainwebMeta_ttl :: Int
  , _chainwebMeta_gasLimit :: Int
  , _chainwebMeta_gasPrice :: Double
  , _chainwebMeta_sender :: Text
  } deriving (Eq,Ord,Show)

instance FromJSON ChainwebMeta where
  parseJSON = withObject "ChainwebMeta" $ \o -> ChainwebMeta
    <$> o .: "chainId"
    <*> o .: "creationTime"
    <*> o .: "ttl"
    <*> o .: "gasLimit"
    <*> o .: "gasPrice"
    <*> o .: "sender"

data BlockPayload = BlockPayload
  { _blockPayload_minerData :: MinerData
  , _blockPayload_transactionsHash :: Hash
  , _blockPayload_outputsHash :: Hash
  , _blockPayload_payloadHash :: Hash
  , _blockPayload_transactions :: [Transaction]
  } deriving (Eq,Show)

instance FromJSON BlockPayload where
  parseJSON = withObject "BlockPayload" $ \o -> BlockPayload
    <$> (fromBase64Url <$> o .: "minerData")
    <*> o .: "transactionsHash"
    <*> o .: "outputsHash"
    <*> o .: "payloadHash"
    <*> (fmap fromBase64Url <$> o .: "transactions")

newtype Base64Url a = Base64Url { fromBase64Url :: a }
  deriving (Eq,Ord,Show)

instance FromJSON a => FromJSON (Base64Url a) where
  parseJSON = withText "Base64Url" $ \t ->
    case decodeB64UrlNoPaddingText t of
      Left e -> fail e
      Right bs -> either fail (pure . Base64Url) $
                    eitherDecode $ BL.fromStrict bs

decodeB64UrlNoPaddingText :: Text -> Either String ByteString
decodeB64UrlNoPaddingText = B64U.decode . T.encodeUtf8 . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="
