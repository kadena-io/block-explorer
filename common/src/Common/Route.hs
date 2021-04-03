{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Common.Route where

------------------------------------------------------------------------------
import           Prelude hiding ((.), id)
import           Control.Category (Category (..))
import           Data.Functor.Identity
import           Data.Map (Map)
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding hiding (Some)
import           Obelisk.Configs
import           Obelisk.Route
import           Obelisk.Route.TH
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Types
------------------------------------------------------------------------------

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()

data BlockRoute :: * -> * where
  Block_Header :: BlockRoute ()
  Block_Transactions :: BlockRoute ()

blockRouteEncoder
  :: Encoder (Either Text) (Either Text) (R BlockRoute) PageName
blockRouteEncoder = pathComponentEncoder $ \case
  Block_Header -> PathEnd $ unitEncoder mempty
  Block_Transactions -> PathSegment "txs" $ unitEncoder mempty

data ChainRoute :: * -> * where
  Chain_BlockHash :: ChainRoute (Text, R BlockRoute)
  Chain_BlockHeight :: ChainRoute (Int, R BlockRoute)

blockIndexRouteEncoder :: Encoder (Either Text) (Either Text) (R ChainRoute) PageName
blockIndexRouteEncoder = pathComponentEncoder $ \case
  Chain_BlockHash -> PathSegment "block" $ pathParamEncoder id blockRouteEncoder
  Chain_BlockHeight -> PathSegment "height" $ pathParamEncoder unsafeTshowEncoder blockRouteEncoder

data NetRoute :: * -> * where
  NetRoute_Chainweb :: NetRoute ()
  NetRoute_Search :: NetRoute ()
  NetRoute_Chain :: NetRoute (Int, R ChainRoute)
  NetRoute_TxReqKey :: NetRoute Text
  NetRoute_TxSearch :: NetRoute (Map Text (Maybe Text))

netRouteEncoder :: Encoder (Either Text) (Either Text) (R NetRoute) PageName
netRouteEncoder = pathComponentEncoder $ \case
  NetRoute_Chainweb -> PathEnd $ unitEncoder mempty
  NetRoute_Search -> PathSegment "search" $ unitEncoder mempty
  NetRoute_Chain -> PathSegment "chain" $ pathParamEncoder unsafeTshowEncoder blockIndexRouteEncoder
  NetRoute_TxReqKey -> PathSegment "tx" singlePathSegmentEncoder
  NetRoute_TxSearch -> PathSegment "txsearch" queryOnlyEncoder

data FrontendRoute :: * -> * where
  FR_Main :: FrontendRoute ()
  FR_About :: FrontendRoute ()
  FR_Mainnet :: FrontendRoute (R NetRoute)
  FR_Testnet :: FrontendRoute (R NetRoute)
  FR_Customnet :: FrontendRoute (Host, R NetRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
backendRouteEncoder = handleEncoder (const (FullRoute_Backend BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    FullRoute_Backend backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    FullRoute_Frontend obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FR_Main -> PathEnd $ unitEncoder mempty
      FR_About -> PathSegment "about" $ unitEncoder mempty
      FR_Mainnet -> PathSegment "mainnet" netRouteEncoder
      FR_Testnet -> PathSegment "testnet" netRouteEncoder
      FR_Customnet -> PathSegment "custom" $ pathParamEncoder hostEncoder netRouteEncoder

hostEncoder :: Encoder (Either Text) (Either Text) Host Text
hostEncoder = reviewEncoder humanReadableTextPrism

addNetRoute :: NetId -> Int -> R ChainRoute -> R FrontendRoute
addNetRoute netId c r = case netId of
  NetId_Mainnet -> FR_Mainnet :/ NetRoute_Chain :/ (c, r)
  NetId_Testnet -> FR_Testnet :/ NetRoute_Chain :/ (c, r)
  NetId_Custom host -> FR_Customnet :/ (host, (NetRoute_Chain :/ (c, r)))

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''BlockRoute
  , ''ChainRoute
  , ''NetRoute
  ]

getAppRoute :: HasConfigs m => m Text
getAppRoute = do
    mroute <- getConfig "common/route"
    case mroute of
      Nothing -> fail "Error getAppRoute: config/common/route not defined"
      Just r -> return $ T.dropWhileEnd (== '/') $ T.strip $ decodeUtf8 r

-- | Provide a human-readable name for a given section
tabTitle :: DomBuilder t m => Some FrontendRoute -> m ()
tabTitle = text . frToText

-- | Provide a human-readable name for a given section
frToText :: Some FrontendRoute -> Text
frToText (Some.Some sec) = case sec of
  FR_Main -> "Home"
  FR_About -> "About"
  FR_Mainnet -> "Block"
  FR_Testnet -> "Block"
  FR_Customnet -> "Block"
