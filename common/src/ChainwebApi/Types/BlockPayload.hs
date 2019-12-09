{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.BlockPayload where

------------------------------------------------------------------------------
import           Data.Aeson
------------------------------------------------------------------------------
import           ChainwebApi.Types.Base64Url
import           ChainwebApi.Types.Hash
import           ChainwebApi.Types.MinerData
import           ChainwebApi.Types.Transaction
------------------------------------------------------------------------------

data BlockPayload = BlockPayload
  { _blockPayload_minerData :: MinerData
  , _blockPayload_transactionsHash :: Hash
  , _blockPayload_outputsHash :: Hash
  , _blockPayload_payloadHash :: Hash
  , _blockPayload_transactions :: [Transaction]
  } deriving (Eq,Show)

instance FromJSON BlockPayload where
  parseJSON = withObject "BlockPayload" $ \o -> BlockPayload
    <$> (fromBase64Url <$> o .: "minerData")
    <*> o .: "transactionsHash"
    <*> o .: "outputsHash"
    <*> o .: "payloadHash"
    <*> (fmap fromBase64Url <$> o .: "transactions")
