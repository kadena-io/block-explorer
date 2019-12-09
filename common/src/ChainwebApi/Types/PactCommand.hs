{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.PactCommand where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------
import ChainwebApi.Types.ChainwebMeta
import ChainwebApi.Types.Payload
import ChainwebApi.Types.Signer
------------------------------------------------------------------------------

data PactCommand = PactCommand
  { _pactCommand_payload :: Payload
  , _pactCommand_signers :: [Signer]
  , _pactCommand_meta :: ChainwebMeta
  , _pactCommand_nonce :: Text
  } deriving (Eq,Show)

instance FromJSON PactCommand where
  parseJSON = withObject "PactCommand" $ \o -> PactCommand
    <$> o .: "payload"
    <*> o .: "signers"
    <*> o .: "meta"
    <*> o .: "nonce"
