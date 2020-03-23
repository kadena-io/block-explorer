{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Page.Types
( PollMetaData(..)
, renderMetaData
, transactionsLink
) where


import Data.Aeson as A
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Chainweb.Api.Common
import Chainweb.Api.Hash

import GHC.Generics (Generic)

-- ------------------------------------------------------------------------ --
-- Chainweb modules

import Chainweb.Api.ChainId

-- ------------------------------------------------------------------------ --
-- Reflex modules

import Common.Route
import Common.Types
import Common.Utils

import Frontend.App
import Frontend.Common

import Obelisk.Route
import Obelisk.Route.Frontend

import Reflex.Dom.Core hiding (Value)



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


transactionsLink
  :: ( MonadApp r t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender js t m
     )
  => NetId
  -> ChainId
  -> Hash
  -> m ()
transactionsLink netId c bhash =
    routeLink route $ text $ hashHex bhash
  where
    route = addNetRoute netId (unChainId c)
      $ Chain_BlockHash :/ (hashB64U bhash) :. Block_Transactions :/ ()

renderMetaData
    :: ( MonadApp r t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Prerender js t m
       )
    => NetId
    -> ChainId
    -> Maybe Value
      -- ^ Value ~ 'PollMetaData'
    -> m ()
renderMetaData _ _ Nothing = text "None"
renderMetaData netId cid (Just v) = case fromJSON v of
    Success (PollMetaData bh bt bhash phash) -> do
      elClass "table" "ui definition table" $ el "tbody" $ do
        tfield "Block Height" $ text $ tshow bh
        tfield "Creation Time" $ text $ tshow $ posixSecondsToUTCTime bt
        tfield "Block Hash" $ transactionsLink netId cid bhash
        tfield "Parent Hash" $ transactionsLink netId cid phash
    A.Error e -> text $ "Unable to decode metadata: " <> T.pack e
