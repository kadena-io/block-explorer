{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Common.Types where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Readable
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.Common
import           Common.Utils
------------------------------------------------------------------------------

type Domain = Text

data Host = Host
  { hostAddress :: Text
  , hostPort    :: Int
  } deriving (Eq,Ord,Show,Read,Generic)

hostScheme :: Host -> Text
hostScheme h = if hostPort h == 80 then "http://" else "https://"

instance ToJSON Host where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Host

instance Humanizable Host where
  humanize (Host a p) = a <> ":" <> tshow p

instance Readable Host where
  fromText t =
      case T.span (/= ':') t of
        ("", _) -> mzero
        (a, rest) -> do
          p <- fromText (T.drop 1 rest)
          return $ Host a p

hostToText :: Host -> Text
hostToText h =
    if hostPort h == 443
      then hostAddress h
      else hostAddress h <> ":" <> tshow (hostPort h)

newtype DataBackends = DataBackends
  { dataBackendMap :: Map Text Host
  } deriving (Eq,Ord,Show,Read)
    deriving newtype (FromJSON, ToJSON)

type ChainwebVersion = Text

data ChainwebHost = ChainwebHost
  { chHost    :: Host
  , chVersion :: ChainwebVersion
  } deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON ChainwebHost where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ChainwebHost

data NetId
   = NetId_Mainnet
   | NetId_Testnet
   | NetId_Custom Host
   deriving (Eq,Ord)

netIdPathSegment :: NetId -> Text
netIdPathSegment = \case
  NetId_Mainnet -> "mainnet"
  NetId_Testnet -> "testnet"
  NetId_Custom _ -> "custom"

netHost :: NetId -> Host
netHost NetId_Mainnet    = Host "estats.chainweb.com" 443
netHost NetId_Testnet    = Host "us1.testnet.chainweb.com" 443
netHost (NetId_Custom h) = h

humanReadableTextPrism :: (Humanizable a, Readable a) => Prism Text Text a a
humanReadableTextPrism = prism' humanize fromText

type Graph = Map Int [Int]
data GraphInfo = GraphInfo
  { giChains :: [ChainId]
  , giGraph :: Graph
  , giShortestPath :: Int -> Int -> Int
  }

data CServerInfo = CServerInfo
  { _csiServerInfo        :: ServerInfo -- TODO use this properly
  , _csiNewestBlockHeight :: BlockHeight
  } deriving (Eq,Ord,Show)

data ServerInfo = ServerInfo
  { _siChainwebVer :: ChainwebVersion
  , _siApiVer      :: Text -- TODO use this properly
  , _siChains      :: Set ChainId
  , _siNumChains   :: Int
  , _siGraphs      :: Maybe [(BlockHeight, [(Int, [Int])])]
  } deriving (Eq,Ord,Show)

siChainsList :: ServerInfo -> [ChainId]
siChainsList = S.toAscList . _siChains

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo
    <$> o .: "nodeVersion"
    <*> o .: "nodeApiVersion"
    <*> o .: "nodeChains"
    <*> o .: "nodeNumberOfChains"
    <*> o .:? "nodeGraphHistory"
