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
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord
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
import           Common.Utils
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Nav
import           Frontend.Page.Block
------------------------------------------------------------------------------


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = appHead
  , _frontend_body = do
      route <- getAppRoute
      curNet <- divClass "ui fixed inverted menu" nav
      _ <- networkView (appWithNetwork route <$> curNet)
      footer
  }

appWithNetwork
  :: (SetRoute t (R FrontendRoute) (Client (Client m)),
      Routed t (R FrontendRoute) (Client m),
      RouteToUrl (R FrontendRoute) (Client (Client m)),
      Prerender js t m, Monad m)
  => Text
  -> Network
  -> m ()
appWithNetwork route curNet = do
  let ch = networkHost curNet
  _ <- prerender blank $ do
    dsi <- getServerInfo ch
    void $ networkView (appWithServer route ch <$> dsi)
  return ()

appWithServer
  :: (DomBuilder t m, Routed t (R FrontendRoute) m, MonadHold t m, MonadFix m,
      Prerender js t m, PostBuild t m, MonadJSM (Performable m),
      HasJSContext (Performable m), PerformEvent t m, TriggerEvent t m,
      RouteToUrl (R FrontendRoute) (Client m), MonadRef m, MonadSample t (Performable m),
      SetRoute t (R FrontendRoute) (Client m))
  => Text
  -> ChainwebHost
  -> Maybe ServerInfo
  -> m ()
appWithServer _ _ Nothing = text "Loading server info..."
appWithServer route ch (Just si) = runApp route ch si mainApp

getServerInfo
  :: (PostBuild t m, TriggerEvent t m, PerformEvent t m,
      HasJSContext (Performable m), MonadJSM (Performable m), MonadHold t m)
  => ChainwebHost
  -> m (Dynamic t (Maybe ServerInfo))
getServerInfo h = do
  pb <- getPostBuild
  ese <- cutToServerInfo <$$$> getCut (h <$ pb)
  holdDyn Nothing ese

mainApp
  :: (MonadApp r t m, Prerender js t m, MonadJSM (Performable m), HasJSContext (Performable m))
  => App (R FrontendRoute) t m ()
mainApp = do
    elAttr "div" ("class" =: "ui main container" <> "style" =: "width: 1127;") $ do
      subRoute_ $ \case
        FR_Main -> blockTableWidget
        FR_Block -> blockPage

footer
  :: (DomBuilder t m)
  => m ()
footer = do
    divClass "ui inverted vertical footer segment" $ do
      el "div" $
        elAttr "img" ("src" =: static @"kadena-full-logo.png" <>
                    "class" =: "ui centered small image" <>
                    "alt" =: "Kadena" ) blank
      divClass "ui divider" blank
      divClass "ui center aligned container" $ do
        divClass "ui stackable inverted divided grid" $ do
          divClass "eight wide column" $ do
            elClass "h4" "ui inverted header" $ text "Company"
            divClass "ui inverted link list" $ do
              lnk "Kadena Website" "https://kadena.io"
              lnk "White Papers" "https://kadena.io/en/whitepapers/"
              lnk "Contact Us" "mailto:info@kadena.io"
          divClass "eight wide column" $ do
            elClass "h4" "ui inverted header" $ text "Social Media"
            divClass "ui inverted link list" $ do
              lnk "Discord" "https://discordapp.com/invite/bsUcWmX"
              lnk "Twitter" "https://twitter.com/kadena_io"
              lnk "Medium" "https://medium.com/kadena-io/"
              lnk "GitHub" "https://github.com/kadena-io/"
              lnk "YouTube" "https://www.youtube.com/KadenaBlockchain"
  where
    lnk nm url = elAttr "a" ("class" =: "item" <> "href" =: url) $ text nm

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

blockTableWidget
  :: (MonadApp r t m, Prerender js t m)
  => App r t m ()
blockTableWidget = do
  pb <- getPostBuild
  void $ prerender blank $ performEvent_ $ liftIO (putStrLn "In blockTableWidget") <$ pb
  divClass "block-table" $ do
    divClass "header-row" $ do
      elClass "span" "table-header" $ text "Height"
      chains <- asks (_siChains . _as_serverInfo)
      forM_ chains $ \cid -> elClass "span" "table-header" $
        text $ "Chain " <> tshow cid

    ti <- prerender (return $ constDyn dummy) $ do
      t <- liftIO getCurrentTime
      clockLossy 1 t
    dbt <- asks _as_blockTable
    _ <- listWithKey (M.mapKeys Down . _blockTable_blocks <$> dbt) (rowsWidget (join ti))
    return ()
  where
    dummy = TickInfo (UTCTime (ModifiedJulianDay 0) 0) 0 0

rowsWidget
  :: (MonadApp r t m, Prerender js t m)
  => Dynamic t TickInfo
  -> Down BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m ()
rowsWidget ti (Down bh) cs = do
  hoverChanges <- blockHeightRow ti bh cs
  hoveredBlock <- holdDyn Nothing hoverChanges
  chains <- asks (_siChains . _as_serverInfo)
  spacerRow chains hoveredBlock

blockHeightRow
  :: (MonadApp r t m, Prerender js t m)
  => Dynamic t TickInfo
  -> BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe ChainId))
blockHeightRow ti height headers = do
  divClass "block-row" $ do
    elClass "span" "block-height" $ text $ tshow height
    chains <- asks (_siChains . _as_serverInfo)
    es <- forM chains $ \cid ->
      blockWidget0 ti height cid headers
    return $ leftmost es

blockWidget0
  :: (MonadApp r t m, Prerender js t m)
  => Dynamic t TickInfo
  -> BlockHeight
  -> ChainId
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe ChainId))
blockWidget0 ti _ cid hs = do
  (e,_) <- elAttr' "span" ("class" =: "summary-details") $ do
    let mbh = M.lookup cid <$> hs
    viewIntoMaybe mbh blank $ \bh -> divClass "summary-inner" $ do
      el "div" $ do
        let mkUrl h = "href" =: ("/block/" <> tshow cid <> "/" <> hashB64U (_blockHeader_hash $ _blockHeaderTx_header h))
        elClass "span" "blockheight" $ elDynAttr "a" (mkUrl <$> bh) $
          dynText $ T.take 8 . hashHex . _blockHeader_hash . _blockHeaderTx_header <$> bh

      let getCreationTime = posixSecondsToUTCTime . _blockHeader_creationTime . _blockHeaderTx_header
      void $ prerender blank $ divClass "blockdiv" $
        pastTimeWidget ti (Just . getCreationTime <$> bh)

      divClass "blockdiv" $ do
        dynText $ maybe "" (\c -> tshow c <> " txs") . _blockHeaderTx_txCount <$> bh

  return $ leftmost [Just cid <$ domEvent Mouseenter e, Nothing <$ domEvent Mouseleave e]

pastTimeWidget
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe UTCTime)
  -> m ()
pastTimeWidget ti dt = do
  let calcDiff lastTick t = maybe "" (diffTimeToSecsAgo . diffUTCTime (_tickInfo_lastUTC lastTick)) t
  dynText (calcDiff <$> ti <*> dt)


diffTimeToSecsAgo :: NominalDiffTime -> Text
diffTimeToSecsAgo delta = tshow (roundInt delta) <> " secs ago"

--diffTimeToRelativeEnglish :: NominalDiffTime -> Text
--diffTimeToRelativeEnglish delta
--  | delta < 5 = "Just now"
--  | delta < oneMinute * 2 = tshow (roundInt delta) <> " secs ago"
--  | delta < oneHour = tshow (roundInt $ delta / oneMinute) <> " min ago"
--  | delta < oneHour * 2 = "an hour ago"
--  | delta < oneDay = tshow (roundInt $ delta / oneHour) <> " hours ago"
--  | delta < oneDay * 2 = "1 day ago"
--  | delta < oneWeek = tshow (roundInt $ delta / oneDay) <> " days ago"
--  | delta < oneWeek * 2 = "1 week ago"
--  | delta < oneMonth = tshow (roundInt $ delta / oneWeek) <> " weeks ago"
--  | delta < oneMonth * 2 = "1 month ago"
--  | delta < oneYear = tshow (roundInt $ delta / oneMonth) <> " months ago"
--  | delta < oneYear * 2 = "a year ago"
--  | otherwise = tshow (roundInt $ delta / oneYear) <> " years ago"

roundInt :: NominalDiffTime -> Int
roundInt = round

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
  let sty = "margin-left: 102px; height: " <> tshow blockSeparation <> "px; " <>
            "border: 0; padding: 0;"
  elAttr "div" ("class" =: "spacer-row" <>
                "style" =: sty ) $ do
    chainweb chains hoveredBlock

svgElDynAttr
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m a
svgElDynAttr elTag attrs child = elDynAttrNS (Just "http://www.w3.org/2000/svg") elTag attrs child

svgElAttr
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Map Text Text
  -> m a
  -> m a
svgElAttr elTag attrs child = svgElDynAttr elTag (constDyn attrs) child

chainweb
  :: (DomBuilder t m, PostBuild t m)
  => [ChainId]
  -> Dynamic t (Maybe ChainId)
  -> m ()
chainweb chains hoveredBlock = do
  svgElAttr "svg" ("viewBox" =: ("0 0 1100 " <> tshow (blockSeparation + 4)) <>
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
                      "y2" =: (tshow (blockSeparation + 4)) ) blank

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
