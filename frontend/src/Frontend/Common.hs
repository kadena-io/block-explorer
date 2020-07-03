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
import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Array.IArray as IA
import           Data.Array.MArray
import           Data.Array.IO
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM as DOM
import qualified "ghcjs-dom" GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as TextArea
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types                   as DOM
import           Language.Javascript.JSaddle (MonadJSM)
import qualified Language.Javascript.JSaddle as JS
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.Common
import           Common.Types
------------------------------------------------------------------------------


(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> a = fmap f <$> a
infixl 4 <$$>

(<$$$>) :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
f <$$$> a = fmap f <$$> a
infixl 4 <$$$>

isPublicKey :: Text -> Bool
isPublicKey t = T.all isHexDigit t && T.length t == 64

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

floydWarshall :: Graph -> [Int]
floydWarshall g = IA.elems arr
  where
    pairs = M.toList g
    vertices = M.keys g
    low = minimum vertices
    high = maximum vertices
    inf = 1000
    arr = runSTUArray $ do
      dist <- newArray ((low,low), (high,high)) inf
      forM_ pairs $ \(u, vs) -> do
        writeArray dist (u,u) 0
        forM_ vs $ \v -> writeArray dist (u,v) 1

      forM_ vertices $ \k ->
        forM_ vertices $ \i ->
          forM_ vertices $ \j -> do
            a <- readArray dist (i,j)
            b <- readArray dist (i,k)
            c <- readArray dist (k,j)
            when (a > b + c) $ do
              writeArray dist (i,j) (b+c)
      return dist

petersenGraph :: Graph
petersenGraph = M.fromList
    [ (0, [2,3,5])
    , (1, [3,4,6])
    , (2, [4,0,7])
    , (3, [0,1,8])
    , (4, [1,2,9])
    , (5, [0,6,9])
    , (6, [1,5,7])
    , (7, [2,6,8])
    , (8, [3,7,9])
    , (9, [4,8,5])
    ]

shortestPath :: Graph -> Int -> Int -> Int
shortestPath g f t = adjs M.! (f * 10 + t)
  where
    adjs = M.fromList $ zip [0..] $ floydWarshall g

rawGraphToGraphInfo :: [(Int, [Int])] -> GraphInfo
rawGraphToGraphInfo adjs = GraphInfo cs (M.fromList adjs)
  where
    cs = S.fromList $ map (ChainId . fst) adjs

type BlockRef = (BlockHeight, ChainId)

isDownstreamFrom :: AllGraphs -> BlockRef -> BlockRef -> Bool
isDownstreamFrom gs (ph, ChainId pc) (h, ChainId c) =
    if h > ph then False else go gs
  where
    go ((gh,GraphInfo _ g):gs)
      | h >= gh = isDownstream g
      | ph >= gh = case M.lookup c g of
                     Nothing -> False
                     Just _ -> isDownstream g
      | otherwise = go gs
    isDownstream g = shortestPath g pc c < ph - h
