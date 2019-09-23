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
module Frontend where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Base64.URL as B64U
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Immutable.Shuffle
import           Language.Javascript.JSaddle.Types
import           Lens.Micro
import           Lens.Micro.Aeson
import           Obelisk.Frontend
import           Obelisk.Configs
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
import           Text.Printf
------------------------------------------------------------------------------
import           Common.Api
import           Common.Route
import           Frontend.App
import           Frontend.ChainwebApi
------------------------------------------------------------------------------


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = appHead
  , _frontend_body = chainScan
  }

chainScan
  :: (MonadApp r t m, Prerender js t m)
  => RoutedT t (R FrontendRoute) m ()
chainScan = do
    elAttr "div" ("class" =: "ui container" <> "style" =: "width: 1100px") $
      subRoute_ $ \case
        FrontendRoute_Main -> recentBlocks
        FrontendRoute_BlockHash -> blockDetails
    return ()

-- Block payload info
-- https://us2.testnet.chainweb.com/chainweb/0.0/testnet02/chain/0/payload/0hz5clj_QCseEvLm4YCrSczgl246fskX_ZAxPixFu6U

-- application/json;blockheader-encoding=object

appHead :: DomBuilder t m => m ()
appHead = do
    el "title" $ text "ChainScan"
    elAttr "link" ("rel" =: "shortcut icon" <>
                   "href" =: "/static/favicon.svg" <>
                   "type" =: "image/svg+xml"
                  ) blank

    css (static @"semantic.min.css")
    css (static @"css/custom.css")
    --jsScript "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.3/jquery.min.js"
    jsScript (static @"jquery-3.1.1.min.js")
    jsScript (static @"semantic.min.js")

css :: DomBuilder t m => Text -> m ()
css url = elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: url) blank

jsScript :: DomBuilder t m => Text -> m ()
jsScript url = elAttr "script" ("src" =: url <> "type" =: "text/javascript") blank

script :: DomBuilder t m =>  Text -> m ()
script code = elAttr "script" ("type" =: "text/javascript") $ text code

leftAndRightBorder :: Text
leftAndRightBorder = "border-left: 1px solid rgba(34,36,38,.1); border-right: 1px solid rgba(34,36,38,.1);"

showResp :: Maybe Value -> String
showResp = show

recentBlocks
  :: (MonadApp r t m, Prerender js t m)
  => m ()
recentBlocks = void $ prerender blank $ do
  pb <- getPostBuild
  let h = Host "us1.testnet.chainweb.com" 443
  ese <- getServerInfo (h <$ pb)
  dse <- holdDyn Nothing ese
  --dynText (maybe "Getting server info..." tshow <$> dse)
  networkView (blockHeaders h <$> dse)
  return ()

numRows :: Int
numRows = 5

blockHeaders
  :: (MonadAppIO r t m, Prerender js t m)
  => Host
  -> Maybe ServerInfo
  -> m ()
blockHeaders _ Nothing = blank
blockHeaders h (Just si) = do
  pb <- getPostBuild
  let mah = _siNewestBlockHeight si
      mih = mah - numRows
  res <- getBlocks2 h (si <$ pb)
  dbt <- holdDyn mempty res
  networkView (blockTableWidget (_siChains si) <$> traceDyn "dbt" dbt)
  return ()


blockTableWidget
  :: (MonadAppIO r t m, Prerender js t m)
  => [ChainId]
  -> BlockTable
  -> m ()
blockTableWidget chains bt = do
  elAttr "table" ("class" =: "ui table" <>
                  "style" =: "border-left: 0; border-right: 0; border-collapse: collapse;") $ do
    --refreshes <- el "thead" $ el "tr" $ do
    --  --el "th" blank
    --  forM [0 :: Int .. 9] $ \c -> do
    --    (e,_) <- elAttr' "th" ("style" =: leftAndRightBorder) $
    --      text $ T.pack $ "Chain " <> show c
    --    return $ ChainId c <$ domEvent Click e

    --el "div" $ text $ tshow $ _blockTable_heights bt
    --el "div" $ text $ tshow $ _blockTable_chainIds bt

    el "tbody" $ do
      t <- liftIO getCurrentTime
      ti <- clockLossy 1 t
      let hs = S.toDescList $ M.keysSet $ _blockTable_blocks bt
      rowsWidget ti chains (take numRows hs) bt

rowsWidget
  :: (MonadAppIO r t m, HasJSContext (Performable m))
  => Dynamic t TickInfo
  -> [ChainId]
  -> [BlockHeight]
  -> BlockTable
  -> m ()
rowsWidget _ _ [] bt = text "No blocks available"
rowsWidget ti chains [bh] bt = void $ blockHeightRow ti chains bt bh
rowsWidget ti chains (bh:rest) bt = do
    hoverChanges <- blockHeightRow ti chains bt bh
    hoveredBlock <- holdDyn Nothing hoverChanges
    spacerRow chains hoveredBlock
    rowsWidget ti chains rest bt

--getRecentBlocks
--  :: (MonadAppIO r t m)
--  => m (Dynamic t (Remote BlockTable))
--getRecentBlocks = do
--  pb <- getPostBuild
--  es <- mapM (\c -> fmapMaybe id <$> getChainBlocks c) (map ChainId [0..9])
--  foldDyn ($) InFlight $ mappend . Landed 1 <$> leftmost es

blockHeightRow
  :: MonadAppIO r t m
  => Dynamic t TickInfo
  -> [ChainId]
  -> BlockTable
  -> BlockHeight
  -> m (Event t (Maybe ChainId))
blockHeightRow ti chains bt bh = do
  elAttr "tr" ("style" =: "margin: 0;") $ do
    --el "td" $ text $ T.pack $ show blockHeight
    es <- forM chains $ \cid ->
      maybe (el "td" $ return never) (blockWidget ti) $ getBlock bh cid bt
    return $ leftmost es

blockWidget :: MonadAppIO r t m => Dynamic t TickInfo -> BlockHeader -> m (Event t (Maybe ChainId))
blockWidget ti (BlockHeader ct _ blockHeight hash chainId _ _ _ _ _ _ _) = do
  (e,_) <- elAttr' "td" ("class" =: "blocksummary") $ do
                         -- <> "style" =: "padding: 0; margin: 0; border: 0; border-spacing: 0;") $ do
    el "div" $ do
      elClass "span" "blockshape" $ text (tshow $ unChainId chainId) --"Bk"
      let url = "/blockHash/" <> hashB64U hash
      elClass "span" "blockheight" $ elAttr "a" ("href" =: url) $
        text $ T.take 8 $ hashHex hash
    --divClass "blockdiv" $ elAttr "a" ("href" =: ("chain/" <> tshow chainId <> "/blockHeight/" <> tshow blockHeight)) $
    --divClass "blockdiv" $ elAttr "a" ("href" =: ("/blockHash/" <> hashB64U hash)) $
    --  text $ "? txs"
    divClass "blockdiv" $ pastTimeWidget ti (posixSecondsToUTCTime ct)

  return $ leftmost [Just chainId <$ domEvent Mouseenter e, Nothing <$ domEvent Mouseleave e]

pastTimeWidget
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadFix m, MonadIO m, MonadIO (Performable m))
  => Dynamic t TickInfo
  -> UTCTime
  -> m ()
pastTimeWidget ti t = do
  --text $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" t
  let calcDiff lastTick = diffUTCTime (_tickInfo_lastUTC lastTick) t
  dynText $ diffTimeToRelativeEnglish . calcDiff <$> ti

diffTimeToRelativeEnglish :: NominalDiffTime -> Text
diffTimeToRelativeEnglish delta
  | delta < 5 = "Just now"
  | delta < oneMinute * 2 = tshow (round delta) <> " secs ago"
  | delta < oneHour = tshow (round $ delta / oneMinute) <> " min ago"
  | delta < oneHour * 2 = "an hour ago"
  | delta < oneDay = tshow (round $ delta / oneHour) <> " hours ago"
  | delta < oneDay * 2 = "1 day ago"
  | delta < oneWeek = tshow (round $ delta / oneDay) <> " days ago"
  | delta < oneWeek * 2 = "1 week ago"
  | delta < oneMonth = tshow (round $ delta / oneWeek) <> " weeks ago"
  | delta < oneMonth * 2 = "1 month ago"
  | delta < oneYear = tshow (round $ delta / oneMonth) <> " months ago"
  | delta < oneYear * 2 = "a year ago"
  | otherwise = tshow (round $ delta / oneYear) <> " years ago"

oneMinute :: NominalDiffTime
oneMinute = 60
oneHour :: NominalDiffTime
oneHour = oneMinute * 60
oneDay :: NominalDiffTime
oneDay = oneHour * 24
oneWeek :: NominalDiffTime
oneWeek = oneDay * 7
oneMonth :: NominalDiffTime
oneMonth = oneDay * 30
oneYear :: NominalDiffTime
oneYear = oneDay * 365

blockWidth :: Int
blockWidth = 110

blockSeparation :: Int
blockSeparation = 50

spacerRow
  :: (DomBuilder t m, PostBuild t m)
  => [ChainId]
  -> Dynamic t (Maybe ChainId)
  -> m ()
spacerRow chains hoveredBlock = do
  elClass "tr" "spacer-row" $ do
    --el "td" $ text ""
    elAttr "td" ("colspan" =: "10" <> "style" =: ("padding: 0; height: " <> tshow blockSeparation <> "px")) $
      chainweb chains hoveredBlock

svgElDynAttr
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m a
svgElDynAttr tag attrs child = elDynAttrNS (Just "http://www.w3.org/2000/svg") tag attrs child

svgElAttr
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Map Text Text
  -> m a
  -> m a
svgElAttr tag attrs child = svgElDynAttr tag (constDyn attrs) child

chainweb
  :: (DomBuilder t m, PostBuild t m)
  => [ChainId]
  -> Dynamic t (Maybe ChainId)
  -> m ()
chainweb chains hoveredBlock = do
  svgElAttr "svg" ("viewBox" =: ("0 0 1100 " <> tshow blockSeparation) <>
                   "style" =: "vertical-align: middle;") $ do
    forM_ chains (linksFromBlock hoveredBlock)
    void $ networkView $ lastLinesForActiveBlock <$> hoveredBlock

lastLinesForActiveBlock :: (DomBuilder t m, PostBuild t m) => Maybe ChainId -> m ()
lastLinesForActiveBlock Nothing = blank
lastLinesForActiveBlock mb@(Just b) = linksFromBlock (constDyn mb) b

fromPos :: Int -> Int
fromPos f = blockWidth `div` 2 + f * blockWidth

toPos :: Int -> Int
toPos t = blockWidth `div` 2 + t * blockWidth

linksFromBlock
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t (Maybe ChainId)
  -> ChainId
  -> m ()
linksFromBlock hoveredBlock f@(ChainId from) = do
  let toBlocks = petersonGraph M.! from
      mkAttrs hb = if hb == Just f
                     then "stroke" =: "rgb(0,0,0)" <>
                          "stroke-width" =: "1.0"
                     else "stroke" =: "rgb(220,220,220)"
  svgElDynAttr "g" (mkAttrs <$> hoveredBlock) $ do
    linkFromTo from from
    mapM_ (linkFromTo from) toBlocks

linkFromTo :: (DomBuilder t m, PostBuild t m) => Int -> Int -> m ()
linkFromTo f t =
    svgElAttr "line" ("x1" =: (tshow $ fromPos f) <>
                      "y1" =: "0" <>
                      "x2" =: (tshow $ toPos t) <>
                      "y2" =: (tshow blockSeparation) ) blank

petersonGraph :: M.Map Int [Int]
petersonGraph = M.fromList
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


blockDetails :: (MonadApp r t m, Routed t Text (Client m), Prerender js t m) => m ()
blockDetails = void $ prerender blank $ do
    pb <- getPostBuild
    jobId <- askRoute
    dynText jobId
    --e <- getBlockHeader ((ChainId 0,) <$> tag (current jobId) pb)
    --blockDyn <- holdDyn Nothing (Just <$> traceEvent "got block data" e)
    --dynText (tshow <$> blockDyn)
    --return ()
