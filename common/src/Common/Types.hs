{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Common.Types where

------------------------------------------------------------------------------
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Readable
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.Common
import           Common.Utils
------------------------------------------------------------------------------

type Domain = Text

data Host = Host
  { hostScheme :: Text
  , hostAddress :: Text
  , hostPort :: Int
  } deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON Host where
    toJSON = String . hostToText
instance FromJSON Host where
    parseJSON = withText "Host" $ \t -> maybe (fail $ T.unpack t <> " is not a valid Host") pure (hostFromText t)

-- These instances are used for route serialization
--instance Humanizable Host where
--  humanize = hostToText
--instance Readable Host where
--  fromText = hostFromText

hostFromText :: MonadPlus m => Text -> m Host
hostFromText t =
    case T.breakOn "://" t of
      (_,"") -> mzero
      (s,hp) -> do
        case T.breakOn ":" (T.drop 3 hp) of
          (a,"") -> case s of
            "http" -> pure $ Host s a 80
            "https" -> pure $ Host s a 443
            _ -> mzero
          (a, rest) -> do
            p <- fromText (T.drop 1 rest)
            return $ Host s a p

hostToText :: Host -> Text
hostToText (Host scheme host port)
  | scheme == "http" && port == 80 = scheme <> "://" <> host
  | scheme == "https" && port == 443 = scheme <> "://" <> host
  | otherwise = scheme <> "://" <> host <> ":" <> tshow port

hostToRouteText :: Host -> Text
hostToRouteText (Host scheme addr port) = T.intercalate "," [scheme, addr, tshow port]

data NetConfig = NetConfig
  { _netConfig_p2pHost :: Host
  , _netConfig_serviceHost :: Host
  , _netConfig_dataHost :: Maybe Host
  } deriving (Eq,Ord,Show,Read)

netConfigFromRouteText :: MonadPlus m => Text -> m NetConfig
netConfigFromRouteText t =
    case T.splitOn "," t of
      [s1,a1,p1,s2,a2,p2,s3,a3,p3] -> NetConfig
        <$> (Host s1 a1 <$> fromText p1)
        <*> (Host s2 a2 <$> fromText p2)
        <*> fmap Just (Host s3 a3 <$> fromText p3)
      [s1,a1,p1,s2,a2,p2] -> NetConfig
        <$> (Host s1 a1 <$> fromText p1)
        <*> (Host s2 a2 <$> fromText p2)
        <*> pure Nothing
      _ -> mzero

netConfigToRouteText :: NetConfig -> Text
netConfigToRouteText (NetConfig p s d) =
  T.intercalate "," [hostToRouteText p, hostToRouteText s, maybe "" hostToRouteText d]

instance ToJSON NetConfig where
    toJSON nc = object
      [ "p2p" .= _netConfig_p2pHost nc
      , "service" .= _netConfig_serviceHost nc
      , "data" .= _netConfig_dataHost nc
      ]
instance FromJSON NetConfig where
    parseJSON = withObject "NetConfig" $ \o -> NetConfig
      <$> o .: "p2p"
      <*> o .: "service"
      <*> o .:? "data"

newtype DataBackends = DataBackends
  { dataBackendMap :: Map Text NetConfig
  } deriving (Eq,Ord,Show,Read)
    deriving newtype (FromJSON, ToJSON)

type ChainwebVersion = Text

data ChainwebHost = ChainwebHost
  { chNetConfig :: NetConfig
  , chVersion :: ChainwebVersion
  } deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON ChainwebHost where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ChainwebHost

data NetId
   = NetId_Mainnet
   | NetId_Testnet
   | NetId_Custom NetConfig
   deriving (Eq,Ord)

netIdPathSegment :: NetId -> Text
netIdPathSegment = \case
  NetId_Mainnet -> "mainnet"
  NetId_Testnet -> "testnet"
  NetId_Custom _ -> "custom"

--getNetConfig :: NetId -> AppConfig -> NetConfig
--getNetConfig NetId_Mainnet    = let h = Host "https" "estats.chainweb.com" 443 in NetConfig h h h
--getNetConfig NetId_Testnet    = let h = Host "https" "api.testnet.chainweb.com" 443 in NetConfig h h h
--getNetConfig (NetId_Custom nc) = nc

netHost :: NetId -> NetConfig
netHost NetId_Mainnet    = let h = Host "https" "estats.chainweb.com" 443 in NetConfig h h (Just h)
netHost NetId_Testnet    = let h = Host "https" "api.testnet.chainweb.com" 443 in NetConfig h h (Just h)
netHost (NetId_Custom nc) = nc

humanReadableTextPrism :: (Humanizable a, Readable a) => Prism Text Text a a
humanReadableTextPrism = prism' humanize fromText

type Graph = Map Int [Int]
data GraphInfo = GraphInfo
  { giChains :: Set ChainId
  , giGraph :: Graph
  , giShortestPaths :: Vector Int
  } deriving (Eq,Ord,Show)

data CServerInfo = CServerInfo
  { _csiServerInfo        :: ServerInfo -- TODO use this properly
  , _csiNewestBlockHeight :: BlockHeight
  } deriving (Eq,Ord,Show)

type AllGraphs = [(BlockHeight, GraphInfo)]

data ServerInfo = ServerInfo
  { _siChainwebVer :: ChainwebVersion
  , _siApiVer      :: Text -- TODO use this properly
  , _siChains      :: Set ChainId
  , _siNumChains   :: Int
  , _siGraphs      :: Maybe [(BlockHeight, [(Int, [Int])])]
  } deriving (Eq,Ord,Show)

getGraphAt :: BlockHeight -> AllGraphs -> GraphInfo
getGraphAt _ [] = error "Empty list of graphs (should never happen)"
getGraphAt _ [(_,g)] = g
getGraphAt bh ((h,g):gs) = if bh >= h then g else getGraphAt bh gs

siCurChains :: BlockHeight -> ServerInfo -> Set ChainId
siCurChains bh si = maybe (_siChains si) (S.fromList . map (ChainId . fst) . snd . head . dropWhile (\(h,_) -> bh < h)) $ _siGraphs si

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo
    <$> o .: "nodeVersion"
    <*> o .: "nodeApiVersion"
    <*> o .: "nodeChains"
    <*> o .: "nodeNumberOfChains"
    <*> o .:? "nodeGraphHistory"
