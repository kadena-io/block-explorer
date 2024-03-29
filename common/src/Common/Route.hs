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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Common.Route where

------------------------------------------------------------------------------
import           Prelude hiding ((.), id)
import           Control.Category (Category (..))
import           Data.Dependent.Sum
import           Data.Functor.Identity
import           Control.Lens hiding ((.=))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding hiding (Some)
import           Obelisk.Configs
import           Obelisk.Route
import           Obelisk.Route.TH
import           Reflex.Dom
import           Text.Read (readMaybe)
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

data AccountParams = AccountParams
  { apToken :: T.Text
  , apAccount :: T.Text
  , apChain :: Maybe Integer
  , apMinHeight :: Maybe Integer
  , apMaxHeight :: Maybe Integer
  }

accountParamsEncoder :: Applicative check =>
  Encoder check (Either T.Text) AccountParams PageName
accountParamsEncoder = unsafeMkEncoder $ EncoderImpl dec enc where
  dec (segments,params) = case segments of
    [account] -> do
      token <- case M.lookup "token" params of
        Nothing -> return "coin"
        Just Nothing -> Left "Expected a value for the token parameter!"
        Just (Just v) -> return v
      chain <- case M.lookup "chain" params of
         Nothing -> return Nothing
         Just Nothing -> Left "Expected a value for the chain parameter!"
         Just (Just chainIdTxt) -> case readMaybe @Integer (T.unpack chainIdTxt) of
             Nothing -> Left $ "Chain \"" <> chainIdTxt <> "\" must be an int"
             Just cid -> Right $ Just cid
      minheight <- case M.lookup "minheight" params of
        Nothing -> return Nothing
        Just Nothing -> Left "Expected a value for the minheight parameter!"
        Just (Just minheightTxt) -> case readMaybe @Integer (T.unpack minheightTxt) of
           Nothing -> Left $ "minheight \"" <> minheightTxt <> "\" must be an int"
           Just minheight -> Right $ Just minheight
      maxheight <- case M.lookup "maxheight" params of
        Nothing -> return Nothing
        Just Nothing -> Left "Expected a value for the maxheight parameter!"
        Just (Just maxheightTxt) -> case readMaybe @Integer (T.unpack maxheightTxt) of
           Nothing -> Left $ "maxheight \"" <> maxheightTxt <> "\" must be an int"
           Just maxheight -> Right $ Just maxheight
      return AccountParams
         {
           apToken = token
         , apAccount = account
         , apChain = chain
         , apMinHeight = minheight
         , apMaxHeight = maxheight
         }
    [] -> Left "Something about no account name in the url."
    _ -> Left "Something about unexpected path segments after the account name."
  enc ap = ([apAccount ap], params) where
    params =
      M.singleton "token" (Just $ apToken ap)
      <> maybe mempty (M.singleton "chain" . Just . T.pack . show)  (apChain ap)
      <> maybe mempty (M.singleton "minheight" . Just . T.pack . show) (apMinHeight ap)
      <> maybe mempty (M.singleton "maxheight" . Just . T.pack . show) (apMaxHeight ap)

data NetRoute :: * -> * where
  NetRoute_Chainweb :: NetRoute ()
  NetRoute_Search :: NetRoute ()
  NetRoute_Chain :: NetRoute (Int, R ChainRoute)
  NetRoute_TxReqKey :: NetRoute Text
  NetRoute_TxDetail :: NetRoute Text
  NetRoute_TxSearch :: NetRoute (Map Text (Maybe Text))
  NetRoute_EventSearch :: NetRoute (Map Text (Maybe Text))
  NetRoute_AccountSearch :: NetRoute AccountParams
  NetRoute_TransferSearch :: NetRoute AccountParams

netRouteEncoder :: Encoder (Either Text) (Either Text) (R NetRoute) PageName
netRouteEncoder = pathComponentEncoder $ \case
  NetRoute_Chainweb -> PathEnd $ unitEncoder mempty
  NetRoute_Search -> PathSegment "search" $ unitEncoder mempty
  NetRoute_Chain -> PathSegment "chain" $ pathParamEncoder unsafeTshowEncoder blockIndexRouteEncoder
  NetRoute_TxReqKey -> PathSegment "tx" singlePathSegmentEncoder
  NetRoute_TxDetail -> PathSegment "txdetail" singlePathSegmentEncoder
  NetRoute_TxSearch -> PathSegment "txsearch" queryOnlyEncoder
  NetRoute_EventSearch -> PathSegment "eventsearch" queryOnlyEncoder
  NetRoute_AccountSearch -> PathSegment "account" accountParamsEncoder
  NetRoute_TransferSearch -> PathSegment "transfer" accountParamsEncoder

data FrontendRoute :: * -> * where
  FR_Main :: FrontendRoute ()
  FR_About :: FrontendRoute ()
  FR_Mainnet :: FrontendRoute (R NetRoute)
  FR_Testnet :: FrontendRoute (R NetRoute)
  FR_Development :: FrontendRoute (R NetRoute)
  FR_FastDevelopment :: FrontendRoute (R NetRoute)
  FR_Customnet :: FrontendRoute (NetConfig, R NetRoute)
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
      FR_Development -> PathSegment "development" netRouteEncoder
      FR_FastDevelopment -> PathSegment "fast-development" netRouteEncoder
      FR_Customnet -> PathSegment "custom" $ pathParamEncoder netconfigEncoder netRouteEncoder

netconfigEncoder :: Encoder (Either Text) (Either Text) NetConfig Text
netconfigEncoder = reviewEncoder (prism' netConfigToRouteText netConfigFromRouteText)

addNetRoute :: NetId -> Int -> R ChainRoute -> R FrontendRoute
addNetRoute netId c r = case netId of
  NetId_Mainnet -> FR_Mainnet :/ NetRoute_Chain :/ (c, r)
  NetId_Testnet -> FR_Testnet :/ NetRoute_Chain :/ (c, r)
  NetId_Development -> FR_Development :/ NetRoute_Chain :/ (c, r)
  NetId_FastDevelopment -> FR_FastDevelopment :/ NetRoute_Chain :/ (c, r)
  NetId_Custom host -> FR_Customnet :/ (host, (NetRoute_Chain :/ (c, r)))

mkNetRoute :: NetId -> DSum NetRoute Identity -> R FrontendRoute
mkNetRoute netId r = case netId of
    NetId_Mainnet -> FR_Mainnet :/ r
    NetId_Testnet -> FR_Testnet :/ r
    NetId_Development -> FR_Development :/ r
    NetId_FastDevelopment -> FR_FastDevelopment :/ r
    NetId_Custom host -> FR_Customnet :/ (host, r)

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
  FR_Development -> "Block"
  FR_FastDevelopment -> "Block"
  FR_Customnet -> "Block"
