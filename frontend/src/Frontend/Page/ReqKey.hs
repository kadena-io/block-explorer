{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.ReqKey where

------------------------------------------------------------------------------
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T (pack)
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core hiding (Value)
import Reflex.Network
import Text.Printf (printf)
------------------------------------------------------------------------------
import Chainweb.Api.ChainId
import Common.Route
import Common.Types
import Common.Utils
import Frontend.App
import Frontend.AppState
import Frontend.ChainwebApi
import Frontend.Common
------------------------------------------------------------------------------

requestKeyWidget
    :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m),
       RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Monad (Client m))
    => ServerInfo
    -> NetId
    -> Int
    -> App Text t m ()
requestKeyWidget si _netId cid = do
    as <- ask
    let n = _as_network as
        chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
        c = ChainId cid

    reqKey <- askRoute

    cmdResult <- fromRequestKey chainwebHost c reqKey
    void $ networkHold  (inlineLoader "Retrieving command result...") $ ffor cmdResult $ \case
      Nothing -> dynText (reqKeyMessage <$> reqKey)
      Just _ -> text "maybe you don't suck"

  where
    reqKeyMessage =
      T.pack . printf "Your request key %s is not associated with an already processed transaction." . show
