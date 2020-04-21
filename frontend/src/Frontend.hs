{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Frontend where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Configs
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Dom.EventSource
import           Reflex.Network
import           Text.Printf
------------------------------------------------------------------------------
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BlockHeaderTx
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainTip
import           Chainweb.Api.Common
import           Chainweb.Api.Cut
import           Chainweb.Api.Hash
import           ChainwebData.Api
import           Common.Route
import           Common.Types
import           Common.Utils
import           Frontend.About
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Nav
import           Frontend.Page.Block
import           Frontend.Page.ReqKey
import           Frontend.Transactions
------------------------------------------------------------------------------

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = appHead
  , _frontend_body = do
      route <- getAppRoute
      ndbs <- getJsonCfg "frontend/data-backends"
      mainDispatch route (either error id ndbs)
      footer
  }

mainDispatch
  :: ObeliskWidget js t (R FrontendRoute) m
  => Text
  -> DataBackends
  -> App (R FrontendRoute) t m ()
mainDispatch route ndbs = do
  pb <- getPostBuild
  subRoute_ $ \case
    FR_Main -> setRoute ((FR_Mainnet :/ NetRoute_Chainweb :/ ()) <$ pb)
    FR_About -> do
      divClass "ui fixed inverted menu" $ nav NetId_Mainnet
      aboutWidget
    FR_Mainnet -> networkDispatch route ndbs NetId_Mainnet
    FR_Testnet -> networkDispatch route ndbs NetId_Testnet
    FR_Customnet -> subPairRoute_ $ \host ->
      networkDispatch route ndbs (NetId_Custom host)

networkDispatch
  :: (ObeliskWidget js t (R FrontendRoute) m)
  => Text
  -> DataBackends
  -> NetId
  -> App (R NetRoute) t m ()
networkDispatch route ndbs netId = prerender_ blank $ do
  divClass "ui fixed inverted menu" $ nav netId
  elAttr "div" ("class" =: "ui main container" <> "style" =: "width: 1124px;") $ do
    dsi <- getServerInfo $ netHost netId
    dyn_ $ ffor dsi $ \case
      Nothing -> inlineLoader "Loading..."
      Just si -> runApp route ndbs netId si $ subRoute_ $ \case
        NetRoute_Chainweb -> do
          as <- ask
          pb <- getPostBuild
          let h = netHost $ _as_network as
          let ch = ChainwebHost h $ _siChainwebVer $ _as_serverInfo as
              f = maximum . map _tipHeight . HM.elems . _cutChains
          height <- f <$$$> getCut (ch <$ pb)
          void $ networkHold (inlineLoader "Getting latest cut...") (mainPageWidget netId <$> height)
        NetRoute_Chain -> chainRouteHandler si netId
        NetRoute_TxReqKey -> requestKeyWidget si netId
        NetRoute_TxSearch -> transactionSearch

chainRouteHandler
  :: (MonadApp r t m, Monad (Client m), MonadJSM (Performable m), HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     )
  => ServerInfo
  -> NetId
  -> App (Int :. R ChainRoute) t m ()
chainRouteHandler si netId = do
    subPairRoute_ $ \cid -> subRoute_ $ \case
      Chain_BlockHash -> blockHashWidget si netId cid
      Chain_BlockHeight -> blockHeightWidget si netId cid

footer
  :: (DomBuilder t m)
  => m ()
footer = do
    divClass "ui inverted vertical footer segment" $ do
      divClass "ui center aligned container" $ do
        elAttr "img" ("src" =: static @"kadena-k-logo.png" <>
                    "class" =: "ui centered mini image" <>
                    "alt" =: "Kadena" ) blank
        divClass "ui horizontal inverted small divided link list" $ do
          lnk "Discord" "https://discordapp.com/invite/bsUcWmX"
          lnk "Twitter" "https://twitter.com/kadena_io"
          lnk "Medium" "https://medium.com/kadena-io/"
          lnk "GitHub" "https://github.com/kadena-io/"
          lnk "YouTube" "https://www.youtube.com/KadenaBlockchain"
  where
    lnk nm url = elAttr "a" ("class" =: "item" <> "href" =: url) $ text nm


getTextCfg :: HasConfigs m => Text -> m (Maybe Text)
getTextCfg p = fmap (T.strip . T.decodeUtf8With T.lenientDecode) <$> getConfig p

getJsonCfg :: (HasConfigs m, FromJSON a) => Text -> m (Either String a)
getJsonCfg p = f <$> getConfig p
  where
    f mbs = do
      bs <- note ("Config file missing: " <> T.unpack p) mbs
      eitherDecodeStrict bs

appHead :: (DomBuilder t m, HasConfigs m) => m ()
appHead = do
    el "title" $ text "Kadena Block Explorer"
    elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "href" =: static @"img/favicon/favicon-96x96.png") blank
    meta ("name" =: "description" <> "content" =: "Block Explorer is an analytics tool for the Kadena platform which visualizes the mining, propagation and braiding of blocks across multiple Kadena chains in real time.")
    meta ("name" =: "keywords" <> "content" =: "kadena, block explorer, mining, propagation, smart contracts, blockchain, chainweb")
    mTrackId <- getTextCfg "frontend/tracking-id"
    case mTrackId of
      Nothing            -> googleAnalyticsTracker "UA-127512784-5"
      Just "no-tracking" -> blank
      Just tid           -> googleAnalyticsTracker tid

    css (static @"semantic.min.css")
    css (static @"css/custom.css")
    --jsScript "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.3/jquery.min.js"
    jsScript (static @"jquery-3.1.1.min.js")
    jsScript (static @"semantic.min.js")
  where
    meta attrs = elAttr "meta" attrs blank


googleAnalyticsTracker :: DomBuilder t m => Text -> m ()
googleAnalyticsTracker gaTrackingId = do
  let gtagSrc = "https://www.googletagmanager.com/gtag/js?id=" <> gaTrackingId
  elAttr "script" ("async" =: "" <> "src" =: gtagSrc) blank
  el "script" $ text $ T.unlines
    [ "window.dataLayer = window.dataLayer || [];"
    , "function gtag(){dataLayer.push(arguments);}"
    , "gtag('js', new Date());"
    , "gtag('config', '" <> gaTrackingId <> "');"
    ]

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

statistic :: (DomBuilder t m) => Text -> m () -> m ()
statistic label val = do
  divClass "ui statistic" $ do
    divClass "value" $ val
    divClass "label" $ text label

initBlockTable
  :: (MonadAppIO r t m, Prerender js t m)
  => BlockHeight
  -> App r t m (Dynamic t BlockTable, Dynamic t GlobalStats, Maybe (Dynamic t RecentTxs))
initBlockTable height = do
    (AppState n si mdbh) <- ask
    let cfg = EventSourceConfig never True
    let ch = ChainwebHost (netHost n) (_siChainwebVer si)
    es <- startEventSource ch cfg
    let downEvent = _eventSource_recv es

    ebt <- getBlockTable ch $ CServerInfo si height

    rec blockTable <- foldDyn ($) mempty $ mergeWith (.)
          [ (<>) <$> ebt
          , (\mbtx bt -> maybe bt (insertBlockTable bt) mbtx) <$> downEvent
          , (\pair bt -> maybe bt (insertBlockTable bt . pairToBhtx) pair) <$> newMissing
          ]

        let missingBlocks = getMissingBlocks blockTable downEvent
            getHeader (cid,hash) = getBlockHeader ch cid (hashB64U hash)
            eme = sequence . fmap getHeader <$> missingBlocks
        ee <- networkHold (return []) eme
        let newMissing = switch (current (leftmost <$> ee))

    let newHrd = attachWith getNewHashrateData (current blockTable) $ fmapMaybe id downEvent
    pb <- getPostBuild
    now <- liftIO getCurrentTime

    let onlyTxs (Just t) = if _blockHeaderTx_txCount t == Just 0 then Nothing else Just t
        onlyTxs Nothing = Nothing
        blocksWithTxs = fmapMaybe onlyTxs downEvent

    (recentTxs, ecds) <- case mdbh of
      Nothing -> return (Nothing, never)
      Just dbh -> do
        eRecentTxs <- getRecentTxs dbh (_eventSource_open es)
        ebp <- getBlockPayload2 ch blocksWithTxs

        recent <- foldDyn ($) (RecentTxs mempty) $ leftmost
          [ either (const id) mergeRecentTxs <$> eRecentTxs
          , addNewTransaction <$> filterRight ebp
          ]

        ecds <- getChainwebStats dbh pb
        return $ (Just recent, ecds)

    stats <- foldDyn ($) (GlobalStats 0 t0 mempty 0 Nothing Nothing) $ mergeWith (.)
      [ maybe id addTxCount <$> downEvent
      , setStartTime now <$ pb
      , addHashrateData <$> filterRight newHrd
      , maybe id addModuleCount <$> downEvent
      , set gs_totalTxCount <$> ((_cds_transactionCount <=< hush) <$> ecds)
      , set gs_circulatingCoins <$> ((_cds_coinsInCirculation <=< hush) <$> ecds)
      ]

    return (blockTable, stats, recentTxs)
  where
    t0 = UTCTime (ModifiedJulianDay 0) 0

data SearchType = RequestKeySearch | TxSearch
  deriving (Eq,Ord,Show,Read,Enum)

searchTypeText :: SearchType -> Text
searchTypeText RequestKeySearch = "Request Key"
searchTypeText TxSearch = "Code"

searchWidget
  :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m,
      SetRoute t (R FrontendRoute) m,
      DomBuilderSpace m ~ GhcjsDomSpace)
  => NetId
  -> App r t m ()
searchWidget netId = do
  divClass "ui fluid action input" $ do
    st <- divClass "ui compact menu search__dropdown" $ do
      divClass "ui simple dropdown item" $ mdo
        curSearchType <- holdDyn RequestKeySearch $ leftmost [rk, txc]
        dynText $ searchTypeText <$> curSearchType
        elClass "i" "dropdown icon" blank
        (rk, txc) <- divClass "menu" $ do
          (r,_) <- elAttr' "div" ("class" =: "item") $ text "Request Key"
          (t,_) <- elAttr' "div" ("class" =: "item") $ text "Code"
          return (RequestKeySearch <$ domEvent Click r, TxSearch <$ domEvent Click t)
        return curSearchType
    ti <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Search term..." <> "style" =: "border-radius: 0;")

    (e,_) <- elAttr' "button" ("class" =: "ui button") $ text "Search"
    setRoute (tag (current $ mkSearchRoute netId <$> value ti <*> st) (domEvent Click e))
    return ()

mkSearchRoute :: NetId -> Text -> SearchType -> R FrontendRoute
mkSearchRoute netId str RequestKeySearch =
  case netId of
    NetId_Mainnet -> FR_Mainnet :/ NetRoute_TxReqKey :/ str
    NetId_Testnet -> FR_Testnet :/ NetRoute_TxReqKey :/ str
    NetId_Custom host -> FR_Customnet :/ (host :. (NetRoute_TxReqKey :/ str))
mkSearchRoute netId str TxSearch = mkTxSearchRoute netId str Nothing

mainPageWidget
  :: forall js r t m. (MonadAppIO r t m, Prerender js t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m,
      DomBuilderSpace m ~ GhcjsDomSpace)
  => NetId
  -> Maybe BlockHeight
  -> App r t m ()
mainPageWidget _ Nothing = text "Error getting cut from server"
mainPageWidget netId (Just height) = do
    si <- asks _as_serverInfo
    (dbt, stats, mrecent) <- initBlockTable height

    searchWidget netId

    t <- liftIO getCurrentTime
    dti <- clockLossy 1 t
    hashrate <- holdDyn Nothing $ attachWith
      (\ti s -> calcNetworkHashrate (utcTimeToPOSIXSeconds $ _tickInfo_lastUTC ti) s)
      (current dti) (updated dbt)
    let getDiff bt c = fmap (blockDifficulty . _blockHeaderTx_header) $ M.lookup c $ _blockTable_cut bt
    let totalDifficulty bt = sum $ catMaybes $ map (getDiff bt) (toList $ _siChains si)

    let statsList :: GlobalStats -> [(Text, Text)]
        statsList s =
            if null slist
              then [("Recent Transactions", tshow $ _gs_txCount s)]
              else slist
          where
            slist = catMaybes
              [ ("Transactions",) . tshow <$> _gs_totalTxCount s
              , ("Circulating Coins",) . siOneDecimal <$> _gs_circulatingCoins s
              ]
    let statAttrs s = "class" =: ("ui mini " <> c <> "statistics")
          where
            c = case length (statsList s) of
                  1 -> "three "
                  2 -> "four "
                  _ -> ""

    _ <- divClass "ui segment" $ do
      elDynAttr "div" (statAttrs <$> stats) $ do
          statistic "Est. Network Hash Rate" (dynText $ maybe "-" ((<>"/s") . diffStr) <$> hashrate)
          statistic "Total Difficulty" (dynText $ diffStr . totalDifficulty <$> dbt)

          networkView $ ffor (statsList <$> stats) $ \ps -> do
            forM ps $ \(n,v) -> statistic n $ text v

    divClass "block-table" $ do
      divClass "header-row" $ do
        elClass "span" "table-header" $ text "Height"
        chains <- asks (_siChains . _as_serverInfo)
        forM_ chains $ \cid -> elClass "span" "table-header" $ do
          el "div" $ text $ "Chain " <> tshow cid
          elAttr "div" ("data-tooltip" =: "The expected number of hashes to mine a block on this chain" <>
                        "data-variation" =: "narrow") $ dynText $ chainDifficulty cid <$> dbt

      rec hoverChanges <- listWithKey (M.mapKeys Down . _blockTable_blocks <$> dbt)
                                      (rowsWidget dti hoveredBlock)
          hoveredBlock <- holdDyn Nothing (switch $ current $ leftmost . M.elems <$> hoverChanges)
      return ()
    case mrecent of
      Nothing -> blank
      Just recent -> void $ networkView (recentTransactions <$> recent)
  where
    _f a = format $ convertToDHMS $ max 0 $ truncate $ diffUTCTime launchTime (_tickInfo_lastUTC a)
    format :: (Int, Int, Int, Int) -> Text
    format (d, h, m, s) = T.pack $ printf "%d days %02d:%02d:%02d" d h m s
    convertToDHMS t =
      let (m', s) = divMod t 60
          (h', m) = divMod m' 60
          (d, h) = divMod h' 24
      in (d, h, m , s)

chainDifficulty :: ChainId -> BlockTable -> Text
chainDifficulty cid bt =
  maybe "" (\b -> diffStr (blockDifficulty $ _blockHeaderTx_header b)) $
    M.lookup cid $ _blockTable_cut bt

rowsWidget
  :: (MonadApp r t m, Prerender js t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe BlockRef)
  -> Down BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe BlockRef))
rowsWidget ti hoveredBlock (Down bh) cs = mdo
  hoverChanges <- blockHeightRow ti hoveredBlock bh cs
  chains <- asks (siChainsList . _as_serverInfo)
  spacerRow chains cs hoveredBlock bh
  return hoverChanges

blockHeightRow
  :: (MonadApp r t m, Prerender js t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe BlockRef)
  -> BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe BlockRef))
blockHeightRow ti hoveredBlock height headers = do
  divClass "block-row" $ do
    elClass "span" "block-height" $ text $ tshow height
    chains <- asks (siChainsList . _as_serverInfo)
    es <- forM chains $ blockWidget0 ti hoveredBlock headers height
    return $ (height,) <$$> leftmost es

blockWidget0
  :: (MonadApp r t m, Prerender js t m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe BlockRef)
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> BlockHeight
  -> ChainId
  -> m (Event t (Maybe ChainId))
blockWidget0 ti hoveredBlock hs height cid = do
  net <- asks _as_network
  let mkAttrs = \case
        Nothing -> "class" =: "summary-details"
        Just hb -> if isDownstreamFrom hb (height, cid)
                     then "class" =: "summary-details hovered-block"
                     else "class" =: "summary-details"
  let mbh = M.lookup cid <$> hs
  (e,_) <- elDynAttr' "span" (mkAttrs <$> hoveredBlock) $ do
    viewIntoMaybe mbh blank $ \bh -> do
      let getHeight = _blockHeader_height . _blockHeaderTx_header
      let mkRoute h = addNetRoute net (unChainId cid) $ Chain_BlockHeight :/ getHeight h :. Block_Header :/ () --TODO: Which NetId should it be?
      dynRouteLink (mkRoute <$> bh) $ divClass "summary-inner" $ do
        el "div" $ do
          elClass "span" "blockheight" $ do
              dynText $ T.take 8 . hashHex . _blockHeader_hash . _blockHeaderTx_header <$> bh

        let getCreationTime = posixSecondsToUTCTime . _blockHeader_creationTime . _blockHeaderTx_header
        void $ prerender blank $ divClass "blockdiv" $
          pastTimeWidget ti (Just . getCreationTime <$> bh)

        divClass "blockdiv" $ do
          dynText $ maybe "" (\c -> tshow c <> " txs") . _blockHeaderTx_txCount <$> bh

        --divClass "blockdiv" $ do
        --  dynText $ diffStr . fromIntegral . targetToDifficulty . leToInteger .
        --            unBytesLE . _blockHeader_target . _blockHeaderTx_header <$> bh

  return $ leftmost [ fmap (const cid) <$> tag (current mbh) (domEvent Mouseenter e)
                    , Nothing <$ domEvent Mouseleave e]

diffStr :: Double -> Text
diffStr d = T.pack $ printf "%.1f %s" (d / divisor) units
  where
    (divisor, units :: String)
      | d >= 1e18 = (1e18, "EH")
      | d >= 1e15 = (1e15, "PH")
      | d >= 1e12 = (1e12, "TH")
      | d >= 1e9 = (1e9, "GH")
      | d >= 1e6 = (1e6, "MH")
      | d >= 1e3 = (1e3, "KH")
      | otherwise = (1, "H")

siOneDecimal :: Double -> Text
siOneDecimal d = T.pack $ printf "%.1f%s" (d / divisor) units
  where
    (divisor, units :: String)
      | d >= 1e18 = (1e18, "E")
      | d >= 1e15 = (1e15, "P")
      | d >= 1e12 = (1e12, "T")
      | d >= 1e9 = (1e9, "G")
      | d >= 1e6 = (1e6, "M")
      | d >= 1e3 = (1e3, "K")
      | otherwise = (1, "")

pastTimeWidget
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t TickInfo
  -> Dynamic t (Maybe UTCTime)
  -> m ()
pastTimeWidget ti dt = do
  let calcDiff lastTick t = maybe "" (diffTimeToSecsAgo . diffUTCTime (_tickInfo_lastUTC lastTick)) t
  dynText (calcDiff <$> ti <*> dt)


diffTimeToSecsAgo :: NominalDiffTime -> Text
diffTimeToSecsAgo delta = tshow (roundInt delta) <> "s ago"

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
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockHeight
  -> m ()
spacerRow chains cs hoveredBlock bh = do
  let sty = "margin-left: 102px; height: " <> tshow blockSeparation <> "px; " <>
            "border: 0; padding: 0;"
  elAttr "div" ("class" =: "spacer-row" <>
                "style" =: sty ) $ do
    chainweb chains cs hoveredBlock bh

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
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockHeight
  -> m ()
chainweb chains cs hoveredBlock bh = do
  let attrs = ("viewBox" =: ("0 0 1100 " <> tshow (blockSeparation + 4)) <>
               "style" =: "vertical-align: middle;")
  svgElAttr "svg" attrs $ do
    forM_ chains (\c -> linksFromBlock cs hoveredBlock (bh, c))
    void $ networkView $ lastLinesForActiveBlock cs hoveredBlock bh <$> hoveredBlock

lastLinesForActiveBlock
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockHeight
  -> Maybe BlockRef
  -> m ()
lastLinesForActiveBlock _ _ _ Nothing = blank
lastLinesForActiveBlock cs hoveredBlock curBH (Just b) =
  if curBH > (fst b) then blank else linksFromBlock cs hoveredBlock b

fromPos :: Int -> Int
fromPos f = blockWidth `div` 2 + f * blockWidth

toPos :: Int -> Int
toPos t = blockWidth `div` 2 + t * blockWidth

linksFromBlock
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockRef
  -> m ()
linksFromBlock cs hoveredBlock fromBlock = do
  let fromChain = unChainId $ snd fromBlock
  let toBlocks = petersonGraph M.! fromChain
      mkAttrs bs mhb = stroke <> (maybe ("style" =: "display: none;") mempty (M.lookup (snd fromBlock) bs))
        where
          stroke =
            case mhb of
              Nothing -> "stroke" =: "rgb(220,220,220)"
              Just hb ->
                if fst fromBlock > fst hb
                  then "stroke" =: "rgb(220,220,220)"
                  else if hb == fromBlock ||
                          isDownstreamFrom hb fromBlock
                         then "stroke" =: "rgb(100,100,100)" <>
                              "stroke-width" =: "1.0"
                         else "stroke" =: "rgb(220,220,220)"
  svgElDynAttr "g" (mkAttrs <$> cs <*> hoveredBlock) $ do
    linkFromTo fromChain fromChain
    mapM_ (linkFromTo fromChain) toBlocks

linkFromTo :: (DomBuilder t m, PostBuild t m) => Int -> Int -> m ()
linkFromTo f t =
    svgElAttr "line" ("x1" =: (tshow $ fromPos f) <>
                      "y1" =: "0" <>
                      "x2" =: (tshow $ toPos t) <>
                      "y2" =: (tshow (blockSeparation + 4)) ) blank

type BlockRef = (BlockHeight, ChainId)

isDownstreamFrom :: BlockRef -> BlockRef -> Bool
isDownstreamFrom (ph, ChainId pc) (h, ChainId c)
  | h > ph = False
  | otherwise = shortestPath pc c <= ph - h

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

shortestPath :: Int -> Int -> Int
shortestPath f t = shortestPaths M.! (f * 10 + t)

shortestPaths :: M.Map Int Int
shortestPaths = M.fromList $ zip [0..]
    [ 0, 2, 1, 1, 2, 1, 2, 2, 2, 2
    , 2, 0, 2, 1, 1, 2, 1, 2, 2, 2
    , 1, 2, 0, 2, 1, 2, 2, 1, 2, 2
    , 1, 1, 2, 0, 2, 2, 2, 2, 1, 2
    , 2, 1, 1, 2, 0, 2, 2, 2, 2, 1
    , 1, 2, 2, 2, 2, 0, 1, 2, 2, 1
    , 2, 1, 2, 2, 2, 1, 0, 1, 2, 2
    , 2, 2, 1, 2, 2, 2, 1, 0, 1, 2
    , 2, 2, 2, 1, 2, 2, 2, 1, 0, 1
    , 2, 2, 2, 2, 1, 1, 2, 2, 1, 0
    ]
