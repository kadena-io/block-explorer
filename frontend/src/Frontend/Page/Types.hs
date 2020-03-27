{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Types
( PollMetaData(..)
) where


import Data.Aeson as A
import Data.Time.Clock.POSIX
import Chainweb.Api.Common
import Chainweb.Api.Hash

import GHC.Generics (Generic)


-- | The shape of enriched polling metadata from chainweb
--
data PollMetaData = PollMetaData
  { _pmd_BlockHeight :: BlockHeight
  , _pmd_CreationTime :: POSIXTime
  , _pmd_BlockHash :: Hash
  , _pmd_PrevBlockHash :: Hash
  } deriving (Eq, Show, Generic)

instance FromJSON PollMetaData where
  parseJSON = withObject "PollMetaData" $ \o -> PollMetaData
    <$> o .: "blockHeight"
    <*> fmap (/ 1000000.0) (o .: "blockTime")
    <*> o .: "blockHash"
    <*> o .: "prevBlockHash"
