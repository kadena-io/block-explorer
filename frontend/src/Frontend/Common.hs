{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.Common where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Data.Maybe
import           Data.Text (Text)
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

tfield :: DomBuilder t m => Text -> m a -> m a
tfield nm v = el "tr" $ do
  el "td" $ text nm
  el "td" v

------------------------------------------------------------------------------
--internalLink
--  :: forall r t m a.
--     (Monad m, DomSpace (DomBuilderSpace m), RouteToUrl r m, SetRoute t r m)
--  => r
--  -> (ElementConfig EventResult t (DomBuilderSpace m) ->
--      m (Element EventResult (DomBuilderSpace m) t, a))
--  -- ^ Probably a call to Reflex.Dom.element
--  -> m ()
--internalLink r wrapper = do
--    enc <- askRouteToUrl
--    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
--          & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
--          & elementConfig_initialAttributes .~ "href" =: enc r
--    (e, _) <- wrapper cfg
--    setRoute $ r <$ domEvent Click e


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
