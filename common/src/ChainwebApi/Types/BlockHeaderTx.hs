{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.BlockHeaderTx where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
------------------------------------------------------------------------------
import           ChainwebApi.Types.BlockHeader
import           ChainwebApi.Types.BlockPayload
------------------------------------------------------------------------------

data BlockHeaderTx = BlockHeaderTx
  { _blockHeaderTx_header :: BlockHeader
  , _blockHeaderTx_txCount :: Maybe Int
  , _blockHeaderTx_powHash :: Maybe Text
  , _blockHeaderTx_target :: Maybe Text
  , _blockHeaderTx_payload :: Maybe BlockPayload
  } deriving (Eq,Show)

instance FromJSON BlockHeaderTx where
  parseJSON = withObject "BlockHeaderTx" $ \o -> BlockHeaderTx
    <$> o .: "header"
    <*> o .: "txCount"
    <*> o .:? "powHash"
    <*> o .:? "target"
    <*> o .:? "payload"
