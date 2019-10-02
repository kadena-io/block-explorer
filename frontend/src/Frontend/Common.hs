{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.Common where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom
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
