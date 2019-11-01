{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Types where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Hashable
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
------------------------------------------------------------------------------
import           Common.Utils
------------------------------------------------------------------------------

type BlockHeight = Int
--newtype BlockHeight = BlockHeight { unBlockHeight :: Int }
--  deriving (Eq,Ord,Enum)
--
--instance Show BlockHeight where
--  show (BlockHeight b) = show b

type Domain = Text

data Host = Host
  { hostAddress :: Text
  , hostPort :: Int
  } deriving (Eq,Ord,Show,Read,Generic)

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
          p <- fromText (T.drop 1 p)
          return $ Host a p
    where
      (a,p) = T.span (/= ':') t

hostToText :: Host -> Text
hostToText h =
    if hostPort h == 443
      then hostAddress h
      else hostAddress h <> ":" <> tshow (hostPort h)

data ChainwebVersion = Development | Testnet02 | Mainnet01
  deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON ChainwebVersion where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ChainwebVersion

versionText :: ChainwebVersion -> Text
versionText Development = "development"
versionText Testnet02 = "testnet02"
versionText Mainnet01 = "mainnet01"

data ChainwebHost = ChainwebHost
  { chHost :: Host
  , chVersion :: ChainwebVersion
  } deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON ChainwebHost where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ChainwebHost

data NetId
   = NetId_Mainnet
   | NetId_Testnet
   | NetId_Custom Host

netHost :: NetId -> Host
netHost NetId_Mainnet = Host "mainnet.example.com" 443
netHost NetId_Testnet = Host "mainnet.example.com" 443
netHost (NetId_Custom h) = h

instance Humanizable NetId where
  humanize NetId_Mainnet = "mainnet.example.com" -- TODO Change this for mainnet
  humanize NetId_Testnet = "us1.testnet.chainweb.com"
  humanize (NetId_Custom ch) = humanize ch

instance Readable NetId where
  fromText "mainnet.example.com" = pure NetId_Mainnet
  fromText "us1.testnet.chainweb.com" = pure NetId_Testnet
  fromText t = NetId_Custom <$> fromText t

humanReadableTextPrism :: (Humanizable a, Readable a) => Prism Text Text a a
humanReadableTextPrism = prism' humanize fromText

newtype ChainId = ChainId { unChainId :: Int }
  deriving (Eq,Ord,Hashable,FromJSONKey,FromJSON)

instance Show ChainId where
  show (ChainId b) = show b

data CServerInfo = CServerInfo
  { _csiServerInfo :: ServerInfo -- TODO use this properly
  , _csiNewestBlockHeight :: BlockHeight
  } deriving (Eq,Ord,Show)

data ServerInfo = ServerInfo
  { _siChainwebVer :: ChainwebVersion
  , _siApiVer :: Text -- TODO use this properly
  , _siChains :: [ChainId]
  , _siNumChains :: Int
  } deriving (Eq,Ord,Show)

--{"nodeNumberOfChains":10
--,"nodeApiVersion":"0.0"
--,"nodeChains":["8","9","4","5","6","7","0","1","2","3"]
--,"nodeVersion":"mainnet01"}

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo
    <$> o .: "nodeVersion"
    <*> o .: "nodeApiVersion"
    <*> o .: "nodeChains"
    <*> o .: "nodeNumberOfChains"
