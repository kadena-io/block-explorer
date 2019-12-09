{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.Signer where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------

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
