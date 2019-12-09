{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.BlockHeaderTx where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import           ChainwebApi.Types.BlockHeader
------------------------------------------------------------------------------

data BlockHeaderTx = BlockHeaderTx
  { _blockHeaderTx_header :: BlockHeader
  , _blockHeaderTx_txCount :: Maybe Int
  , _blockHeaderTx_powHash :: Maybe Text
  , _blockHeaderTx_target :: Maybe Text
  } deriving (Eq,Ord,Show)

instance FromJSON BlockHeaderTx where
  parseJSON = withObject "BlockHeaderTx" $ \o -> BlockHeaderTx
    <$> o .: "header"
    <*> o .: "txCount"
    <*> o .:? "powHash"
    <*> o .:? "target"
