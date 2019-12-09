{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.Sig where

import Data.Aeson
import Data.Text (Text)

newtype Sig = Sig { unSig :: Text }
  deriving (Eq,Show)

instance FromJSON Sig where
  parseJSON = withObject "Sig" $ \o -> Sig
    <$> o .: "sig"
