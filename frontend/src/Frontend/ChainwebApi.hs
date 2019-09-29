{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}

module Frontend.ChainwebApi where

------------------------------------------------------------------------------
import           Debug.Trace (traceShow, traceShowId)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64U
import           Data.Either
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Lens.Micro
import           Lens.Micro.Aeson
import           Reflex.Dom hiding (Cut, Value)
import           Text.Printf
------------------------------------------------------------------------------
import           Frontend.Common
------------------------------------------------------------------------------

--data Remote a = InFlight | Crashed [String] | Landed a
--
--instance Show a => Show (Remote a) where
--  show InFlight = "Request in flight"
--  show (Landed _) = "Request landed"
--  show (Crashed es) = unlines $
--    "Request crashed with the following errors:" : es
--
--instance Functor Remote where
--  fmap f (Landed a) = Landed (f a)
--  fmap _ (Crashed es) = Crashed es
--  fmap _ InFlight = InFlight
--
--instance Semigroup a => Semigroup (Remote a) where
--  InFlight <> InFlight = InFlight
--  InFlight <> Crashed es = Crashed es
--  Crashed es <> InFlight = Crashed es
--  Crashed es1 <> Crashed es2 = Crashed (es1 <> es2)
--  InFlight <> Landed a = Landed a
--  Crashed es <> Landed a = Crashed es
--  Landed a <> InFlight = Landed a
--  Landed a <> Crashed es = Crashed es
--  Landed a1 <> Landed a2 = Landed (a1 <> a2)
--
--instance Semigroup a => Monoid (Remote a) where
--  mempty = InFlight

--instance Applicative Remote where
--  pure a = Landed a
--  Landed f <*> Landed a = Landed (f a)
--  Crashed s1 <*> Crashed s2 = Crashed (s1 <> s2)
--  _ <*> Crashed s = Crashed s
--  Crashed s <*> _ = Crashed s
--  _ <*> InFlight = InFlight

--remoteToMaybe :: Remote a -> Maybe a
--remoteToMaybe (Landed a) = Just a
--remoteToMaybe _ = Nothing
--
--remoteToEither :: Remote a -> Either [String] a
--remoteToEither InFlight = Left ["In flight"]
--remoteToEither (Crashed s) = Left s
--remoteToEither (Landed a) = Right a

type BlockHeight = Int
--newtype BlockHeight = BlockHeight { unBlockHeight :: Int }
--  deriving (Eq,Ord,Enum)
--
--instance Show BlockHeight where
--  show (BlockHeight b) = show b

newtype ChainId = ChainId { unChainId :: Int }
  deriving (Eq,Ord,Hashable,FromJSONKey, FromJSON)

instance Show ChainId where
  show (ChainId b) = show b

data Host = Host
  { hostAddress :: Text
  , hostPort :: Int
  } deriving (Eq,Ord,Show)

data ChainwebVersion = Development | Testnet02 | Mainnet01
  deriving (Eq,Ord,Show)

versionText :: ChainwebVersion -> Text
versionText Development = "development"
versionText Testnet02 = "testnet02"
versionText Mainnet01 = "mainnet01"

data ChainwebHost = ChainwebHost
  { chHost :: Host
  , chVersion :: ChainwebVersion
  } deriving (Eq,Ord,Show)

apiBaseUrl :: ChainwebHost -> Text
apiBaseUrl (ChainwebHost h cver) =
    T.pack $ printf "https://%s/chainweb/0.0/%s/" (T.unpack host) (T.unpack $ versionText cver)
  where
    host = if hostPort h == 443
             then hostAddress h
             else hostAddress h <> ":" <> tshow (hostPort h)

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

data ServerInfo = ServerInfo
  { _siApiVer :: Text -- TODO use this properly
  , _siChains :: [ChainId]
  , _siNewestBlockHeight :: BlockHeight
  } deriving (Eq,Ord,Show)

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

cutToServerInfo :: Cut -> ServerInfo
cutToServerInfo c = ServerInfo "0.0" (sort $ HM.keys chains) h
  where
    chains = _cutChains c
    h = maximum $ map _tipHeight $ HM.elems chains

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
  -> ServerInfo
  -> m (Event t BlockTable)
getBlockTable h si = do
  pb <- getPostBuild
  resp <- performRequestsAsync $ mkHeaderRequest h si <$ pb
  return (foldl' (\bt val -> combineBlockTables bt val) mempty <$> (fmap decodeXhrResponse <$> resp))

getBlockHeader
  :: (MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => ChainwebHost
  -> ChainId
  -> Text
  -> m (Event t (Maybe BlockHeader))
getBlockHeader h c blockHash = do
  pb <- getPostBuild
  resp <- performRequestAsync $ mkSingleHeaderRequest h c blockHash <$ pb
  return (decodeXhrResponse <$> resp)

combineBlockTables :: BlockTable -> Maybe Value -> BlockTable
combineBlockTables bt Nothing = bt
combineBlockTables bt (Just v) = foldl' (\bt b -> insertBlockTable bt (BlockHeaderTx 0 b)) bt $ rights $
  map (parseEither parseJSON) $ getItems v

mkHeaderRequest :: ChainwebHost -> ServerInfo -> [XhrRequest ()]
mkHeaderRequest h si = map (\c -> (XhrRequest "GET" (headersUrl h minh maxh c) cfg))
                         $ _siChains si
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }
    maxh = _siNewestBlockHeight si
    minh = maxh - blockTableNumRows

mkSingleHeaderRequest :: ChainwebHost -> ChainId -> Text -> XhrRequest ()
mkSingleHeaderRequest h c blockHash = XhrRequest "GET" (headerUrl h c blockHash) cfg
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }

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

data BlockHeaderTx = BlockHeaderTx
  { _blockHeaderTx_txCount :: Int
  , _blockHeaderTx_header :: BlockHeader
  } deriving (Eq,Ord,Show)

instance FromJSON BlockHeaderTx where
  parseJSON = withObject "BlockHeaderTx" $ \o -> BlockHeaderTx
    <$> o .: "txCount"
    <*> o .: "header"

data BlockHeader = BlockHeader
  { _blockHeader_creationTime :: POSIXTime
  , _blockHeader_parent :: Hash
  , _blockHeader_height :: BlockHeight
  , _blockHeader_hash :: Hash
  , _blockHeader_chainId :: ChainId
  , _blockHeader_weight :: Text
  , _blockHeader_epochStart :: POSIXTime
  , _blockHeader_neighbors :: Map ChainId Hash
  , _blockHeader_payloadHash :: Hash
  , _blockHeader_chainwebVer :: Text
  , _blockHeader_target :: Text
  , _blockHeader_nonce :: Text
  } deriving (Eq,Ord,Show)

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
  } deriving (Eq,Ord)

instance Show BlockTable where
  show (BlockTable bs) = unlines $ map show $ M.keys bs

instance Semigroup BlockTable where
  (BlockTable b1) <> (BlockTable b2) = BlockTable (M.unionWith M.union b1 b2)

instance Monoid BlockTable where
  mempty = BlockTable mempty

blockTableNumRows :: Int
blockTableNumRows = 5

insertBlockTable :: BlockTable -> BlockHeaderTx -> BlockTable
insertBlockTable (BlockTable bs) btx = BlockTable bs2
  where
    b = _blockHeaderTx_header btx
    h = _blockHeader_height b
    c = _blockHeader_chainId b
    bs2 = M.delete (h - blockTableNumRows) $ M.alter f h bs

    f Nothing = Just $ M.singleton c btx
    f (Just m) = Just $ M.insert c btx m

getBlock :: BlockHeight -> ChainId -> BlockTable -> Maybe BlockHeaderTx
getBlock bh cid bt = M.lookup cid =<< M.lookup bh (_blockTable_blocks bt)

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

decodeB64UrlNoPaddingText :: Text -> Either String ByteString
decodeB64UrlNoPaddingText = B64U.decode . T.encodeUtf8 . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="
