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


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = appHead
  , _frontend_body = do
      route <- getAppRoute
      --let h = Host "us1.testnet.chainweb.com" 443
      --let h = Host "localhost" 60651
      let h = Host "us3.tn1.chainweb.com" 443
          ch = ChainwebHost h Development
      void $ prerender blank $ do
        dsi <- getServerInfo ch
        void $ networkView (appWithServer route ch <$> dsi)
      --void $ prerender blank $ runApp route ch (chainScan ch)
  }

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
appWithServer route ch (Just si) = runApp route ch si (chainScan ch)

getServerInfo
  :: (PostBuild t m, TriggerEvent t m, PerformEvent t m,
      HasJSContext (Performable m), MonadJSM (Performable m), MonadHold t m)
  => ChainwebHost
  -> m (Dynamic t (Maybe ServerInfo))
getServerInfo h = do
  pb <- getPostBuild
  ese <- cutToServerInfo <$$$> getCut (h <$ pb)
  holdDyn Nothing ese

chainScan
  :: (MonadApp r t m, Prerender js t m, MonadJSM (Performable m), HasJSContext (Performable m))
  => ChainwebHost
  -> RoutedT t (R FrontendRoute) m ()
chainScan ch = do
    --elAttr "div" ("class" =: "ui container" <> "style" =: "width: 1100px; overflow-x: auto;") $ do
    elAttr "div" ("class" =: "ui container" <> "style" =: "width: 1200px;") $ do
    dsi <- fmap join $ prerender (text "prerendering server info" >> return (constDyn Nothing)) $ getServerInfo ch
    void $ networkView (mainApp ch <$> dsi)


mainApp
  :: (MonadApp r t m, Prerender js t m, MonadJSM (Performable m), HasJSContext (Performable m))
  => ChainwebHost
  -> Maybe ServerInfo
  -> App (R FrontendRoute) t m ()
mainApp _ Nothing = text "Loading server data..."
mainApp ch (Just si) = do
    subRoute_ $ \case
      FR_Main -> blockTableWidget
      FR_Block -> blockDetails

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

blockTableWidget
  :: (MonadApp r t m, Prerender js t m)
  => App r t m ()
blockTableWidget = do
  pb <- getPostBuild
  void $ prerender blank $ performEvent_ $ liftIO (putStrLn "In blockTableWidget") <$ pb
  elAttr "table" ("id" =: "blockheaders" <>
                  "class" =: "ui definition table" <>
                  "style" =: "border-left: 0; border-right: 0; border-collapse: collapse;") $ do
    el "thead" $ do
      el "tr" $ do
        el "th" blank
        chains <- asks (_siChains . _as_serverInfo)
        --let sty = "width: " <> tshow blockWidth <> "px;"
        forM_ chains $ \cid -> el "th" $
          text $ "Chain " <> tshow cid
    elClass "tbody" "chainwebtable" $ do
      ti <- prerender (return $ constDyn dummy) $ do
        t <- liftIO getCurrentTime
        clockLossy 1 t
      dbt <- asks _as_blockTable
      listWithKey (M.mapKeys Down . _blockTable_blocks <$> dbt) (rowsWidget (join ti))
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
    pb <- getPostBuild
    void $ prerender blank $ performEvent_ $ liftIO (putStrLn "In rowsWidget") <$ pb
    hoverChanges <- blockHeightRow ti bh cs
    hoveredBlock <- holdDyn Nothing hoverChanges
    spacerRow hoveredBlock

--getRecentBlocks
--  :: (MonadAppIO r t m)
--  => m (Dynamic t (Remote BlockTable))
--getRecentBlocks = do
--  pb <- getPostBuild
--  es <- mapM (\c -> fmapMaybe id <$> getChainBlocks c) (map ChainId [0..9])
--  foldDyn ($) InFlight $ mappend . Landed 1 <$> leftmost es

blockHeightRow
  :: (MonadApp r t m, Prerender js t m)
  => Dynamic t TickInfo
  -> BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe ChainId))
blockHeightRow ti height headers = do
  elAttr "tr" ("style" =: "margin: 0;") $ do
    el "td" $ text $ tshow height
    chains <- asks (_siChains . _as_serverInfo)
    es <- forM chains $ \cid ->
      blockWidget0 ti height cid headers
      --maybe (el "td" $ return never) (blockWidget ti) $ M.lookup cid <$> header
    return $ leftmost es

blockWidget0
  :: (MonadApp r t m, Prerender js t m)
  => Dynamic t TickInfo
  -> BlockHeight
  -> ChainId
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe ChainId))
blockWidget0 ti height cid hs = do
  (e,_) <- elAttr' "td" ("class" =: "blocksummary"
                      <> "style" =: ("width: " <> tshow blockWidth)) $ do
    let mbh = M.lookup cid <$> hs
    el "div" $ do
      --elClass "span" "blockshape" $ text (tshow $ unChainId cid) --"Bk"
      let mkUrl h = "href" =: ("/block/" <> tshow cid <> "/" <> hashB64U (_blockHeader_hash $ _blockHeaderTx_header h))
      elClass "span" "blockheight" $ elDynAttr "a" (maybe mempty mkUrl <$> mbh) $
        dynText $ maybe "" (T.take 8 . hashHex . _blockHeader_hash . _blockHeaderTx_header) <$> mbh
    --divClass "blockdiv" $ elAttr "a" ("href" =: ("chain/" <> tshow chainId <> "/blockHeight/" <> tshow blockHeight)) $
    divClass "blockdiv" $ do --elAttr "a" ("href" =: ("/blockHash/" <> hashB64U hash)) $
      dynText $ maybe "" ((<> " txs") . tshow . _blockHeaderTx_txCount) <$> mbh
    let getCreationTime = posixSecondsToUTCTime . _blockHeader_creationTime . _blockHeaderTx_header
    prerender blank $ divClass "blockdiv" $
      pastTimeWidget ti (fmap getCreationTime <$> mbh)

  return $ leftmost [Just cid <$ domEvent Mouseenter e, Nothing <$ domEvent Mouseleave e]

--blockWidget :: MonadAppIO r t m => Dynamic t TickInfo -> BlockHeader -> m (Event t (Maybe ChainId))
--blockWidget ti (BlockHeader ct _ _ hash chainId _ _ _ _ _ _ _) = do
--  (e,_) <- elAttr' "td" ("class" =: "blocksummary") $ do
--                         -- <> "style" =: "padding: 0; margin: 0; border: 0; border-spacing: 0;") $ do
--    el "div" $ do
--      elClass "span" "blockshape" $ text (tshow $ unChainId chainId) --"Bk"
--      let url = "/blockHash/" <> hashB64U hash
--      elClass "span" "blockheight" $ elAttr "a" ("href" =: url) $
--        text $ T.take 8 $ hashHex hash
--    --divClass "blockdiv" $ elAttr "a" ("href" =: ("chain/" <> tshow chainId <> "/blockHeight/" <> tshow blockHeight)) $
--    --divClass "blockdiv" $ elAttr "a" ("href" =: ("/blockHash/" <> hashB64U hash)) $
--    --  text $ "? txs"
--    divClass "blockdiv" $ pastTimeWidget ti (posixSecondsToUTCTime ct)
--
--  return $ leftmost [Just chainId <$ domEvent Mouseenter e, Nothing <$ domEvent Mouseleave e]

pastTimeWidget
  -- :: (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadFix m, MonadIO m, MonadIO (Performable m))
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe UTCTime)
  -> m ()
pastTimeWidget ti dt = do
  --text $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S" t
  let calcDiff lastTick t = maybe "" (diffTimeToRelativeEnglish . diffUTCTime (_tickInfo_lastUTC lastTick)) t
  dynText (calcDiff <$> ti <*> dt)

diffTimeToRelativeEnglish :: NominalDiffTime -> Text
diffTimeToRelativeEnglish delta
  | delta < 5 = "Just now"
  | delta < oneMinute * 2 = tshow (roundInt delta) <> " secs ago"
  | delta < oneHour = tshow (roundInt $ delta / oneMinute) <> " min ago"
  | delta < oneHour * 2 = "an hour ago"
  | delta < oneDay = tshow (roundInt $ delta / oneHour) <> " hours ago"
  | delta < oneDay * 2 = "1 day ago"
  | delta < oneWeek = tshow (roundInt $ delta / oneDay) <> " days ago"
  | delta < oneWeek * 2 = "1 week ago"
  | delta < oneMonth = tshow (roundInt $ delta / oneWeek) <> " weeks ago"
  | delta < oneMonth * 2 = "1 month ago"
  | delta < oneYear = tshow (roundInt $ delta / oneMonth) <> " months ago"
  | delta < oneYear * 2 = "a year ago"
  | otherwise = tshow (roundInt $ delta / oneYear) <> " years ago"

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
  :: (MonadApp r t m)
  => Dynamic t (Maybe ChainId)
  -> m ()
spacerRow hoveredBlock = do
  elClass "tr" "spacer-row" $ do
    elClass "td" "emptyrowheader" $ text ""
    let sty = "padding: 0; border-left: 0; height: " <> tshow blockSeparation <> "px"
    elAttr "td" ("colspan" =: "10" <> "style" =: sty) $
      chainweb hoveredBlock

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
  :: (MonadApp r t m)
  => Dynamic t (Maybe ChainId)
  -> m ()
chainweb hoveredBlock = do
  svgElAttr "svg" ("viewBox" =: ("0 0 1100 " <> tshow blockSeparation) <>
                   "style" =: "vertical-align: middle;") $ do
    chains <- asks (_siChains . _as_serverInfo)
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


blockDetails
  :: (MonadApp r t m, Monad (Client m), MonadJSM (Performable m), HasJSContext (Performable m))
  => App [Text] t m ()
blockDetails = do
    args <- askRoute
    void $ networkView (blockWidget <$> args)

blockWidget
  :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m))
  => [Text]
  -> m ()
blockWidget args = do
  as <- ask
  case args of
    [chainId, hash] -> do
      case fromText chainId of
        Nothing -> text $ "Invalid chain ID: " <> chainId
        Just cid -> do
          e <- getBlockHeader (_as_host as) (ChainId cid) hash
          blockDyn <- holdDyn Nothing e
          void $ networkView (blockHeaderWidget <$> blockDyn)
    _ -> text "Must pass chain ID and block hash"


blockHeaderWidget
  :: (MonadApp r t m)
  => Maybe BlockHeader
  -> m ()
blockHeaderWidget Nothing = text "Block does not exist"
blockHeaderWidget (Just b) = do
  elAttr "table" ("class" =: "ui definition table") $ do
    el "tbody" $ do
      field "Creation Time" $ text . tshow . posixSecondsToUTCTime . _blockHeader_creationTime
      field "Chain" $ text . tshow . _blockHeader_chainId
      field "Block Height" $ text . tshow . _blockHeader_height
      field "Parent" $ text . hashHex . _blockHeader_parent
      field "Hash" $ text . hashHex . _blockHeader_hash
      field "Weight" $ text . _blockHeader_weight
      field "Epoch Start" $ text . tshow . _blockHeader_epochStart
      field "Neighbors" $ neighbors . _blockHeader_neighbors
      field "Payload Hash" $ text . hashHex . _blockHeader_payloadHash
      field "Chainweb Version" $ text . _blockHeader_chainwebVer
      field "Target" $ text . _blockHeader_target
      field "Nonce" $ text . _blockHeader_nonce
      return ()
  where
    field nm func = el "tr" $ do
      el "td" $ text nm
      el "td" $ func b
    neighbors ns = el "ul" $ do
      forM_ (M.toList ns) $ \(cid,nh) -> do
        el "li" $ text $ "Chain " <> tshow cid <> ": " <> nh
