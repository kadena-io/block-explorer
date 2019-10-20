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

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

------------------------------------------------------------------------------
--import           Prelude hiding ((.), id)
--import           Control.Category (Category (..))
import           Control.Lens
import           Control.Monad.Except
import           Data.Map (Map)
import           Data.Readable
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Functor.Sum
import           Obelisk.Configs
import           Obelisk.Route
import           Obelisk.Route.TH
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Utils
------------------------------------------------------------------------------

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Hashes :: BackendRoute (Map Text (Maybe Text))

data BlockRoute :: * -> * where
  Block_Header :: BlockRoute ()
  Block_Transactions :: BlockRoute ()

blockRouteEncoder
  :: Encoder (Either Text) (Either Text) (R BlockRoute) PageName
blockRouteEncoder = pathComponentEncoder $ \case
  Block_Header -> PathEnd $ unitEncoder mempty
  Block_Transactions -> PathSegment "txs" $ unitEncoder mempty


blockRouteEncoder2
  :: Encoder (Either Text) (Either Text) (Int, Text, R BlockRoute) PageName
blockRouteEncoder2 = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \(path, _query) ->
      case path of
        (cidStr : hash : rest) -> do
          cid <- maybe (Left "Could not parse chain id") Right $ fromText cidStr
          br <- case rest of
                  [] -> Right Block_Header
                  ["txs"] -> Right Block_Transactions
                  _ -> throwError $ "blockRouteEncoder: Invalid path " <> tshow path
          pure (cid, hash, br :/ ())
        l -> throwError $ "singletonListEncoderImpl: expected one item, got " <> tshow (length l)
  , _encoderImpl_encode = \(cid, hash, br :/ _) ->
      case br of
        Block_Header -> ([tshow cid, hash], mempty)
        Block_Transactions -> ([tshow cid, hash, "txs"], mempty)
  }

--blockRouteToPath :: BlockRoute () -> [Text]
--blockRouteToPath Block_Header = []
--blockRouteToPath Block_Transactions = ["txs"]

--blockRouteEncoder
--  :: Int
--  -> Text
--  -> Encoder (Either Text) (Either Text) (R BlockRoute) PageName
--blockRouteEncoder chainId blockHash = pathComponentEncoder $ \case
--  Block_Header -> PathEnd $ unitEncoder mempty
--  Block_Transactions -> PathSegment "txs" $ unitEncoder mempty

data FrontendRoute :: * -> * where
  FR_Main :: FrontendRoute ()
  FR_Block :: FrontendRoute (Int, Text, R BlockRoute)
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

pathOnlyEncoderIgnoringQuery :: (Applicative check, MonadError Text parse) => Encoder check parse [Text] PageName
pathOnlyEncoderIgnoringQuery = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \(path, _query) -> pure path
  , _encoderImpl_encode = \path -> (path, mempty)
  }

--singletonListEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse a [a]
--singletonListEncoder = unsafeMkEncoder $ EncoderImpl
--  { _encoderImpl_decode = \case
--      [a] -> pure a
--      l -> throwError $ "singletonListEncoderImpl: expected one item, got " <> tshow (length l)
--  , _encoderImpl_encode = (:[])
--  }


backendRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
backendRouteEncoder = handleEncoder (const (FullRoute_Backend BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    FullRoute_Backend backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      --BackendRoute_Hashes -> PathSegment "hash" singlePathSegmentEncoder
      BackendRoute_Hashes -> PathSegment "hash" queryOnlyEncoder -- (pathOnlyEncoderIgnoringQuery . singletonListEncoder)
    FullRoute_Frontend obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FR_Main -> PathEnd $ unitEncoder mempty
      FR_Block -> PathSegment "block" blockRouteEncoder2
        --pathSegmentEncoder . bimap unwrappedEncoder (maybeEncoder (unitEncoder mempty) blockRouteEncoder)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''BlockRoute
  ]

getAppRoute :: HasConfigs m => m Text
getAppRoute = do
    mroute <- getConfig "common/route"
    case mroute of
      Nothing -> fail "Error getAppRoute: config/common/route not defined"
      Just r -> return $ T.dropWhileEnd (== '/') $ T.strip $ decodeUtf8 r

-- | Provide a human-readable name for a given section
tabTitle :: DomBuilder t m => Some FrontendRoute -> m ()
tabTitle sfr@(Some.Some sec) = case sec of
  FR_Main -> text $ frToText sfr
  FR_Block -> text $ frToText sfr

-- | Provide a human-readable name for a given section
frToText :: Some FrontendRoute -> Text
frToText (Some.Some sec) = case sec of
  FR_Main -> "Home"
  FR_Block -> "Block"
