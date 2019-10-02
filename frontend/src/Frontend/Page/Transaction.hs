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
module Frontend.Page.Transaction where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord
import           Data.Readable
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
------------------------------------------------------------------------------

transactionPage
  :: (MonadApp r t m)
  => BlockPayload
  -> m ()
transactionPage bp = do
    el "h2" $ text "Transaction"
    forM_ (_blockPayload_transactions bp) $ \t -> do
      divClass "ui button" $ elClass "i" "icon plus" blank
      divClass "ui segment tx-preview" $ el "pre" $ text $ payloadCode $ _pactCommand_payload $ _transaction_cmd t
