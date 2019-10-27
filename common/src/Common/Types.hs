{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Types where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
------------------------------------------------------------------------------
import           Common.Utils
------------------------------------------------------------------------------

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
