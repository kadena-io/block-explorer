{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.ChainwebApi where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64U
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Readable
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Aeson
import           Reflex.Dom hiding (Cut, Value)
import           Text.Printf
------------------------------------------------------------------------------
import           Frontend.App
------------------------------------------------------------------------------

data Remote a = InFlight | Crashed [String] | Landed a

instance Show a => Show (Remote a) where
  show InFlight = "Request in flight"
  show (Landed _) = "Request landed"
  show (Crashed es) = unlines $
    "Request crashed with the following errors:" : es

instance Functor Remote where
  fmap f (Landed a) = Landed (f a)
  fmap _ (Crashed es) = Crashed es
  fmap _ InFlight = InFlight

instance Semigroup a => Semigroup (Remote a) where
  InFlight <> InFlight = InFlight
  InFlight <> Crashed es = Crashed es
  Crashed es <> InFlight = Crashed es
  Crashed es1 <> Crashed es2 = Crashed (es1 <> es2)
  InFlight <> Landed a = Landed a
  Crashed es <> Landed a = Crashed es
  Landed a <> InFlight = Landed a
  Landed a <> Crashed es = Crashed es
  Landed a1 <> Landed a2 = Landed (a1 <> a2)

instance Semigroup a => Monoid (Remote a) where
  mempty = InFlight

--instance Applicative Remote where
--  pure a = Landed a
--  Landed f <*> Landed a = Landed (f a)
--  Crashed s1 <*> Crashed s2 = Crashed (s1 <> s2)
--  _ <*> Crashed s = Crashed s
--  Crashed s <*> _ = Crashed s
--  _ <*> InFlight = InFlight

remoteToMaybe :: Remote a -> Maybe a
remoteToMaybe (Landed a) = Just a
remoteToMaybe _ = Nothing

remoteToEither :: Remote a -> Either [String] a
remoteToEither InFlight = Left ["In flight"]
remoteToEither (Crashed s) = Left s
remoteToEither (Landed a) = Right a

chainwebVersion = "testnet02"

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

apiBaseUrl :: Host -> Text -> Text
apiBaseUrl h cver = T.pack $ printf "https://%s/chainweb/0.0/%s/" (T.unpack host) (T.unpack cver)
  where
    host = if hostPort h == 443
             then hostAddress h
             else hostAddress h <> ":" <> tshow (hostPort h)

cutUrl :: Host -> Text -> Text
cutUrl h cver = apiBaseUrl h cver <> "cut"

chainBaseUrl :: Host -> ChainId -> Text
chainBaseUrl h chainId = apiBaseUrl h chainwebVersion <> "chain/" <> (tshow (unChainId chainId))

chainHashesUrl :: Host -> BlockHeight -> BlockHeight -> ChainId -> Text
chainHashesUrl h minHeight maxHeight chainId = chainBaseUrl h chainId <>
  "/hash?minheight=" <> tshow minHeight <> "&maxheight=" <> tshow maxHeight

headersUrl :: Host -> BlockHeight -> BlockHeight -> ChainId -> Text
headersUrl h minHeight maxHeight chainId = chainBaseUrl h chainId <>
  "/header?minheight=" <> tshow minHeight <> "&maxheight=" <> tshow maxHeight

data ServerInfo = ServerInfo
  { _siApiVer :: Text -- TODO use this properly
  , _siChainwebVer :: Text
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
cutToServerInfo c = ServerInfo "0.0" chainwebVersion (sort $ HM.keys chains) h
  where
    chains = _cutChains c
    h = maximum $ map _tipHeight $ HM.elems chains

getServerInfo
  :: (MonadAppIO r t m)
  => Event t Host
  -> m (Event t (Maybe ServerInfo))
getServerInfo host = do
  cutToServerInfo <$$$> getCut host

getCut
  :: (MonadAppIO r t m)
  => Event t Host
  -> m (Event t (Maybe Cut))
getCut host = do
  resp <- performRequestsAsync $ fmap (\h -> (h, XhrRequest "GET" (cutUrl h chainwebVersion) def)) host
  return (decodeXhrResponse . snd <$> traceEventWith (maybe "" T.unpack . _xhrResponse_responseText . snd) resp)

--getItems :: Value -> [Text]
--getItems val = val ^.. key "items" . values . _String

getItems :: Value -> [Value]
getItems val = val ^.. key "items" . values

--mkBlockTable :: ((BlockHeight,BlockHeight,ChainId), XhrResponse) -> Maybe BlockTable
--mkBlockTable ((mih, mah, chainId), resp) = do
--    v <- decodeXhrResponse resp
--    return $ mconcat $ map singletonBlockTable $ getItems v
--
--getChainBlocks
--  :: (MonadAppIO r t m)
--  => Host
--  -> Event t (BlockHeight, BlockHeight, ChainId)
--  -> m (Event t (Maybe BlockTable))
--getChainBlocks h chainId = do
--  resp <- performRequestsAsync $ fmap (\a@(mih, mah, cid) -> (a, XhrRequest "GET" (chainHashesUrl h mih mah cid) def)) chainId
--  return (mkBlockTable <$> traceEventWith (maybe "" T.unpack . _xhrResponse_responseText . snd) resp)

getBlocks2
  :: (MonadAppIO r t m)
  => Host
  -> Event t ServerInfo
  -> m (Event t BlockTable)
getBlocks2 h si = do
  resp <- performRequestsAsync $ fmap (mkHeaderRequest2 h) si
  --return (mkBlockTable <$> traceEventWith (maybe "" T.unpack . _xhrResponse_responseText . snd) resp)
  return (foldl' combineBlockTables2 mempty <$> (fmap decodeXhrResponse <$> resp))

combineBlockTables :: BlockTable -> XhrResponse -> BlockTable
combineBlockTables bt resp = case decodeXhrResponse resp of
                Nothing -> bt
                Just v -> foldl' insertBlockTable bt $ catMaybes $ map (parseMaybe parseJSON) $ getItems v

combineBlockTables2 :: BlockTable -> Maybe Value -> BlockTable
combineBlockTables2 bt Nothing = bt
combineBlockTables2 bt (Just v) = foldl' insertBlockTable bt $ catMaybes $ map (parseMaybe parseJSON) $ getItems v

mkHeaderRequest :: Host -> ServerInfo -> [((BlockHeight,BlockHeight,ChainId), XhrRequest ())]
mkHeaderRequest h si = map (\c -> ((minh, maxh, c), XhrRequest "GET" (chainHashesUrl h minh maxh c) cfg))
                         $ _siChains si
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }
    maxh = _siNewestBlockHeight si
    minh = maxh - 10

mkHeaderRequest2 :: Host -> ServerInfo -> [XhrRequest ()]
mkHeaderRequest2 h si = map (\c -> (XhrRequest "GET" (headersUrl h minh maxh c) cfg))
                         $ _siChains si
  where
    cfg = def { _xhrRequestConfig_headers = "accept" =: "application/json;blockheader-encoding=object" }
    maxh = _siNewestBlockHeight si
    minh = maxh - 10

--getBlockHeader
--  :: (MonadAppIO r t m)
--  => Event t (ChainId, Text)
--  -> m (Event t Text)
--getBlockHeader cidAndHash = do
--  resp <- performRequestsAsync $ fmap mkPair cidAndHash
--  let rawText = fmap (T.dropWhile (=='"') . T.dropWhileEnd (=='"')) . _xhrResponse_responseText . snd <$> resp
--
--  -- decodeText . T.decodeUtf8 <=<
--  return $ fmapMaybe id $ traceEvent "block details" ((decodeB64UrlNoPaddingText =<<) <$> traceEvent "rawText" rawText)
--  --return $ traceEvent "block details" (mkBlockDetails <$> resp)
--  where
--    mkPair (cid, hash) = (cid, XhrRequest "GET" (blockHeaderUrl cid hash) def)

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

{-
data:{
"txCount":0,
"header":{
  "creationTime":1569418573005922,
  "parent":"ifz24oPHu-ttUpHcOsjWLoZ_wWxRzuogYkooxP-abQ8",
  "height":84,
  "hash":"ZUjXyjY2ObDy6Ybv74R7qsbS2WZjtJlp7mtE_QX4MVY",
  "chainId":6,
  "weight":"Jz2JAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
  "epochStart":1569418573005922,
  "adjacents":{"7":"BxN7dkdn0YwuSs0-B1mLzMLIbyf_OuAkngPVkuFDY4M","1":"PqlYkCQqfHrrnvW5bhSv0mJYCsQz3ewXZ44QA2bANnI","5":"t5355Z7I_QLZ24gPhAxaGUru3BpeBAvl3dBLdbDW_kA"}
  ,"payloadHash":"j5YYGad134kwmceK4Bvh-65ivs1eJBYf8meHHgjwO2w"
  ,"chainwebVersion":"testnet02"
  ,"target":"VB6tT6O7wHdoQ3HgVgmmjrj-gK4nnzPND0N74TgGAAA"
  ,"nonce":"5630980"}
}
-}

data BlockHeader = BlockHeader
  { _blockHeader_creationTime :: POSIXTime
  , _blockHeader_parent :: Hash
  , _blockHeader_height :: BlockHeight
  , _blockHeader_hash :: Hash
  , _blockHeader_chainId :: ChainId
  , _blockHeader_weight :: Text
  , _blockHeader_epochStart :: POSIXTime
  , _blockHeader_neighbors :: Map ChainId Text
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
    <*> (o .: "hash")
    <*> o .: "chainId"
    <*> (o .: "weight")
    <*> (fmap (/1000000.0) $ o .: "epochStart")
    <*> o .: "adjacents"
    <*> (o .: "payloadHash")
    <*> o .: "chainwebVersion"
    <*> o .: "target"
    <*> o .: "nonce"

data BlockTable = BlockTable
  { _blockTable_blocks :: Map BlockHeight (Map ChainId BlockHeader)
  } deriving (Eq,Ord)

instance Show BlockTable where
  show (BlockTable bs) = unlines $ map show $ M.keys bs

instance Semigroup BlockTable where
  (BlockTable b1) <> (BlockTable b2) = BlockTable (b1 <> b2)

instance Monoid BlockTable where
  mempty = BlockTable mempty

insertBlockTable :: BlockTable -> BlockHeader -> BlockTable
insertBlockTable (BlockTable bs) b = BlockTable bs2
  where
    h = _blockHeader_height b
    c = _blockHeader_chainId b
    bs2 = M.delete (h-10) $ M.alter f h bs

    -- TODO This constant 10 should be passed in
    f Nothing = Just $ M.singleton c b
    f (Just m) = Just $ M.insert c b m

getBlock :: BlockHeight -> ChainId -> BlockTable -> Maybe BlockHeader
getBlock bh cid bt = M.lookup cid =<< M.lookup bh (_blockTable_blocks bt)

hush :: Either e a -> Maybe a
hush (Left _) = Nothing
hush (Right a) = Just a

decodeB64UrlNoPaddingText :: Text -> Either String ByteString
decodeB64UrlNoPaddingText = B64U.decode . T.encodeUtf8 . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="
