{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Common.Api where

import Language.Javascript.JSaddle (MonadJSM)
import Reflex.TriggerEvent.Class (TriggerEvent)
import Reflex.PerformEvent.Class (Performable, PerformEvent)
import Reflex.Dom.Builder.Class (DomBuilderSpace, RawElement)
import qualified GHCJS.DOM.Types as DOM

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

-- | Constraints required to implement route click widgets.
--
-- These widgets support Ctrl-clicking ie opening a new tab when Ctrl key is pressed while they are clicked.
-- Upon clicking normally (without Ctrl), they use client side routing.
type RouteClick t m =
  ( MonadJSM m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadJSM (Performable m)
  , DOM.IsEventTarget (RawElement (DomBuilderSpace m))
  )
