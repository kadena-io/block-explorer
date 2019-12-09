{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.ChainwebMeta where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX
------------------------------------------------------------------------------

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
