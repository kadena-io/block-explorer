{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.Common where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types                   as DOM
import           Language.Javascript.JSaddle (MonadJSM)
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------


(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> a = fmap f <$> a
infixl 4 <$$>

(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
f <$$$> a = fmap f <$$> a
infixl 4 <$$$>

data ListState = EmptyPlaceholder | AddForm | ListTable
  deriving (Eq,Ord,Show,Read)

extLink :: DomBuilder t m => Text -> m a -> m a
extLink href m =
  elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener") $ m

detailsSection :: DomBuilder t m => m a -> m a
detailsSection c = do
  elClass "table" "ui fixed single line definition table" $ el "tbody" c

tfield :: DomBuilder t m => Text -> m a -> m a
tfield nm v = el "tr" $ do
  elClass "td" "two wide" $ text nm
  el "td" v

viewIntoMaybe
    :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
    => Dynamic t (Maybe a)
    -> m b
    -> (Dynamic t a -> m b)
    -> m (Event t b)
viewIntoMaybe dm nothingWidget justWidget = do
  dmd <- distributeMaybe dm
  networkView $ maybe nothingWidget justWidget <$> dmd

holdIntoMaybe
    :: (DomBuilder t m, MonadHold t m, MonadFix m)
    => Dynamic t (Maybe a)
    -> m b
    -- ^ Widget to use as the initial value
    -> m b
    -- ^ Widget to use in the Nothing case
    -> (Dynamic t a -> m b)
    -> m (Dynamic t b)
holdIntoMaybe dm initialWidget nothingWidget justWidget = do
  dmd <- distributeMaybe dm
  networkHold initialWidget $ maybe nothingWidget justWidget <$> updated dmd

distributeMaybe
    :: forall t a m. (Reflex t, MonadFix m, MonadHold t m)
    => Dynamic t (Maybe a)
    -> m (Dynamic t (Maybe (Dynamic t a)))
distributeMaybe d = do
  ma <- sample $ current d
  let inner :: forall m'. (MonadFix m', MonadHold t m') => a -> m' (Dynamic t a)
      inner a = holdDyn a . fmapMaybe id =<< takeWhileE isJust (updated d)
  mInner0 :: Maybe (Dynamic t a) <- mapM inner ma
  holdDyn mInner0 $ flip push (updated d) $ \new -> do
    old <- sample $ current d
    if isJust old == isJust new then return Nothing else Just <$> mapM inner new

inlineLoader :: DomBuilder t m => Text -> m ()
inlineLoader msg = divClass "ui active centered inline text loader" $ text msg

-- -- | Copies the text content of a given node.
-- --
-- --   Probably won't work for input elements.
-- copyButton
--   :: forall t m
--   . (DynamicButtonConstraints t m, MonadJSM (Performable m), PerformEvent t m)
--   => UiButtonDynCfg t
--   -> Behavior t Text
--   -> m (Event t ())
-- copyButton cfg t = do
--     onClick <- uiButtonDyn (cfg & uiButtonCfg_class <>~ "button_border_none") $ do
--       elClass "i" "fa fa-lg fa-copy fa-fw" blank
--       dyn_ $ ffor (cfg ^. uiButtonCfg_title) $ \case
--         Nothing -> blank
--         Just title -> text title
--     _ <- copyToClipboard $ tag t onClick
--     pure onClick

-- | Copy the given text to the clipboard
copyToClipboard
  :: (MonadJSM (Performable m), PerformEvent t m)
  => Event t Text
  -- ^ Text to copy to clipboard. Event must come directly from user
  -- interaction (e.g. domEvent Click), or the copy will not take place.
  -> m (Event t Bool)
  -- ^ Did the copy take place successfully?
copyToClipboard copy = performEvent $ ffor copy $ \t -> do
  doc <- DOM.currentDocumentUnchecked
  ta <- DOM.uncheckedCastTo TextArea.HTMLTextAreaElement <$> Document.createElement doc ("textarea" :: Text)
  TextArea.setValue ta t
  body <- Document.getBodyUnchecked doc
  _ <- Node.appendChild body ta
  HTMLElement.focus ta
  TextArea.select ta
  success <- Document.execCommand doc ("copy" :: Text) False (Nothing :: Maybe Text)
  _ <- Node.removeChild body ta
  pure success
