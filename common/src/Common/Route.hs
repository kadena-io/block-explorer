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
{-# LANGUAGE TypeOperators #-}

module Common.Route where

------------------------------------------------------------------------------
import           Prelude hiding ((.), id)
import           Control.Category (Category (..))
import           Control.Categorical.Bifunctor
import           Control.Monad.Except
import           Data.Functor.Identity
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

------------------------------------------------------------------------------
-- START: Move to Obelisk.Route

infixr 5 :.
type (:.) = (,)

{-# COMPLETE (:.) #-}
pattern (:.) :: a -> b -> a :. b
pattern a :. b = (a, b)

addPathSegmentEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse (Text, PageName) PageName
addPathSegmentEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(ph, (pt, q)) -> (ph : pt, q)
  , _encoderImpl_decode = \(p, q) -> case p of
      [] -> throwError "Expected a path segment"
      ph : pt -> pure (ph, (pt, q))
  }

pathParamEncoder
  :: forall check parse item rest.
     ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse item Text
  -> Encoder check parse rest PageName
  -> Encoder check parse (item :. rest) PageName
pathParamEncoder itemUnchecked restUnchecked = addPathSegmentEncoder . bimap itemUnchecked restUnchecked

-- END: Move to Obelisk.Route
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
  FR_About :: FrontendRoute ()
  FR_Block :: FrontendRoute (Int :. Text :. R BlockRoute)
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
    FullRoute_Frontend obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FR_Main -> PathEnd $ unitEncoder mempty
      FR_About -> PathSegment "about" $ unitEncoder mempty
      FR_Block -> PathSegment "block" $ pathParamEncoder unsafeTshowEncoder $ pathParamEncoder id blockRouteEncoder

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
tabTitle sfr@(Some.Some sec) = text $ frToText sfr

-- | Provide a human-readable name for a given section
frToText :: Some FrontendRoute -> Text
frToText (Some.Some sec) = case sec of
  FR_Main -> "Home"
  FR_About -> "About"
  FR_Block -> "Block"
