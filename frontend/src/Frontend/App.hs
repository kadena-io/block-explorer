{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.App where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Base64.URL as B64U
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Immutable.Shuffle
import           Language.Javascript.JSaddle.Types
import           Lens.Micro
import           Lens.Micro.Aeson
import           Obelisk.Frontend
import           Obelisk.Configs
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
import           Text.Printf
------------------------------------------------------------------------------
import           Common.Api
import           Common.Route
------------------------------------------------------------------------------

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> a = fmap f <$> a
infixl 4 <$$>

(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
f <$$$> a = fmap f <$$> a
infixl 4 <$$$>


tshow :: Show a => a -> Text
tshow = T.pack . show

type MonadApp r t m =
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadRef m
  , PerformEvent t m
  , TriggerEvent t m
--  , MonadReader (AppState t) m
--  , EventWriter t AppTriggers m
  , SetRoute t (R FrontendRoute) (Client m)
  , RouteToUrl (R FrontendRoute) (Client m)
  )

type MonadAppIO r t m =
  ( MonadApp r t m
  , MonadIO m
  , MonadIO (Performable m)
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )

type App r t m a = RoutedT t r m a
--type App r t m a =
--    RoutedT t r (ReaderT (AppState t) (EventWriterT t AppTriggers m)) a
