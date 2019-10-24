{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Dependent.Sum (DSum ((:=>)))
import           Obelisk.Backend
import           Obelisk.Route
import           Snap.Core
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Common.Route
------------------------------------------------------------------------------

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve serveBackendRoute
  , _backend_routeEncoder = backendRouteEncoder
  }


serveBackendRoute :: R BackendRoute -> Snap ()
serveBackendRoute br = do
  liftIO $ putStrLn "Incoming request"
  case br of
    _ -> pure ()
