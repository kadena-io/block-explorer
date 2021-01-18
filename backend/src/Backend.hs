{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend (backend) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Obelisk.Backend
import           Obelisk.Route
import           Snap.Core
------------------------------------------------------------------------------
import           Common.Route
import           Frontend                  (frontend)
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
