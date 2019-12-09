{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.RespItems where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
------------------------------------------------------------------------------

data RespItems a = RespItems
  { _respItems_next :: Text
  , _respItems_items :: [a]
  , _respItems_limit :: Int
  } deriving (Eq,Ord,Show)

instance FromJSON a => FromJSON (RespItems a) where
  parseJSON = withObject "RespItems" $ \o -> RespItems
    <$> o .: "next"
    <*> o .: "items"
    <*> o .: "limit"
