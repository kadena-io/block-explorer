{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.App where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Text (Text)
import           Language.Javascript.JSaddle.Types
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types
import           Frontend.AppState
------------------------------------------------------------------------------

type MonadApp r t m =
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadRef m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadReader (AppState t) m
  , EventWriter t AppTriggers m
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

runApp
--  :: (DomBuilder t m, Routed t r m, MonadHold t m, MonadFix m, Prerender js t m, PostBuild t m, MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m)
  :: (DomBuilder t m, Routed t r m, MonadFix m)
  => Text
  -> NetId
  -> ServerInfo
  -> RoutedT t r (ReaderT (AppState t) (EventWriterT t AppTriggers m)) a
  -> m a
runApp publicUrl net si m = mdo
    r <- askRoute
    as <- stateManager publicUrl net si triggers
    (res, triggers) <- runEventWriterT (runReaderT (runRoutedT m r) as)
    return res
