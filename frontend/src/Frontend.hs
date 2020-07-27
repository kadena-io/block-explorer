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
import           Data.Bifunctor
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Vector as V
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
import           Chainweb.Api.BlockPayload
import           Chainweb.Api.BlockPayloadWithOutputs
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainTip
import           Chainweb.Api.Common
import           Chainweb.Api.Cut
import           Chainweb.Api.Hash
import           Chainweb.Api.MinerData
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
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m,
      Prerender js t m
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
    (AppState n si mdbh _) <- ask
    let cfg = EventSourceConfig never True
    let ch = ChainwebHost (netHost n) (_siChainwebVer si)
    es <- startEventSource ch cfg
    let downEvent = _eventSource_recv es

    ebt <- getBlockTable ch $ CServerInfo si height

    rec blockTable <- foldDyn ($) mempty $ mergeWith (.)
          [ (<>) <$> ebt
          , (\mbtx bt -> maybe bt (insertBlockTable bt) mbtx) <$> downEvent
          , (\pair bt -> maybe bt (insertBlockTable bt . pairToBhtx) pair) <$> newMissing
          , insertPayload <$> switch (current payloadUpdates)
          ]

        let missingBlocks = getMissingBlocks blockTable downEvent
            getHeader (cid,hash) = getBlockHeader ch cid (hashB64U hash)
            eme = sequence . fmap getHeader <$> missingBlocks
        ee <- networkHold (return []) eme
        let newMissing = switch (current (leftmost <$> ee))
            newBlocks = leftmost
              [ _blockHeaderTx_header <$$> downEvent
              , fst <$$> newMissing
              ]
        payloadUpdates <- networkHold (return never) $ getPayload ch <$> fmapMaybe id newBlocks

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

    stats <- foldDyn ($) (GlobalStats 0 t0 mempty 0 Nothing Nothing Nothing) $ mergeWith (.)
      [ maybe id addTxCount <$> downEvent
      , setStartTime now <$ pb
      , addHashrateData <$> filterRight newHrd
      , maybe id addModuleCount <$> downEvent
      , set gs_totalTxCount <$> ((_cds_transactionCount <=< hush) <$> ecds)
      , set gs_circulatingCoins <$> ((_cds_coinsInCirculation <=< hush) <$> ecds)
      , set gs_possibleCoins <$> ((fmap _cds_maxPossibleCoins . hush) <$> ecds)
      ]

    return (blockTable, stats, recentTxs)
  where
    t0 = UTCTime (ModifiedJulianDay 0) 0

insertPayload :: Either String (BlockPayload, BlockHeader) -> BlockTable -> BlockTable
insertPayload (Left e) _ = error $ "Error getting block payload: " <> e
insertPayload (Right (p,h)) bt = addPayloadToTable (_blockHeader_height h) (_blockHeader_chainId h) p bt

getPayload
  :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m))
  => ChainwebHost
  -> BlockHeader
  -> m (Event t (Either String (BlockPayload, BlockHeader)))
getPayload ch h = do
  e <- getBlockPayload ch (_blockHeader_chainId h) (_blockHeader_payloadHash h)
  return $ (,h) <$$> e

data SearchType = RequestKeySearch | TxSearch
  deriving (Eq,Ord,Show,Read,Enum)

searchTypeText :: SearchType -> Text
searchTypeText RequestKeySearch = "Request Key"
searchTypeText TxSearch = "Code"

searchWidget
  :: (PostBuild t m, PerformEvent t m, DomBuilder t m,
      DomBuilder t m, MonadHold t m, MonadFix m,
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
    appState <- ask
    let si = _as_serverInfo appState
    (dbt, stats, mrecent) <- initBlockTable height
    let maxBlockHeight = blockTableMaxHeight <$> dbt

    searchWidget netId

    t <- liftIO getCurrentTime
    dti <- clockLossy 1 t
    hashrate <- holdDyn Nothing $ attachWith
      (\ti s -> calcNetworkHashrate (utcTimeToPOSIXSeconds $ _tickInfo_lastUTC ti) s)
      (current dti) (updated dbt)
    let getDiff bt c = fmap (blockDifficulty . _blockHeaderTx_header) $ M.lookup c $ _blockTable_cut bt
    let totalDifficulty bt = sum $ catMaybes $ map (getDiff bt) (toList $ siCurChains (blockTableMaxHeight bt) si)

    let statsList :: GlobalStats -> [(Text, Text)]
        statsList s =
            if null slist
              then [("Recent Transactions", tshow $ _gs_txCount s)]
              else slist
          where
            slist = catMaybes
              [ ("Transactions",) . tshow <$> _gs_totalTxCount s
              , ("Circulating Coins",) . siOneDecimal <$> _gs_circulatingCoins s
              , ("Possible Coins",) . siOneDecimal <$> _gs_possibleCoins s
              ]
        statAttrs s = "class" =: ("ui mini " <> c <> "statistics")
          where
            c = case length (statsList s) of
                      1 -> "three "
                      2 -> "four "
                      3 -> "five "
                      _ -> ""

    _ <- divClass "ui segment" $ do
      elDynAttr "div" (statAttrs <$> stats) $ do
          statistic "Est. Network Hash Rate" (dynText $ maybe "-" ((<>"/s") . diffStr) <$> hashrate)
          statistic "Total Difficulty" (dynText $ diffStr . totalDifficulty <$> dbt)

          networkView $ ffor (statsList <$> stats) $ \ps -> do
            forM ps $ \(n,v) -> statistic n $ text v

    dChains <- holdUniqDyn $ (\h -> Set.toList $ siCurChains h si) <$> maxBlockHeight
    _ <- networkView $ ffor dChains $ \widestChains -> do
      let maxNumChains = length widestChains
      divClass "block-table" $ do
        divClass "header-row" $ do
          elClass "span" "table-header block-height" $ text "Height"
          let mkTooltip cid bt = "class" =: "table-header" <>
                if maxNumChains <= 10
                  then mempty
                  else "data-tooltip" =: chainDifficulty cid bt <>
                       "data-variation" =: "narrow"

          forM_ widestChains $ \cid -> elDynAttr "span" (mkTooltip cid <$> dbt) $ do
            el "div" $ text $ "Chain " <> tshow cid
            when (maxNumChains <= 10) $
              elAttr "div" ("data-tooltip" =: "The expected number of hashes to mine a block on this chain" <>
                            "data-variation" =: "narrow") $ dynText $ chainDifficulty cid <$> dbt

        let defaultGraphs = if maxNumChains == 10
                             then [(0, GraphInfo (Set.fromList widestChains) petersenGraph (V.fromList $ floydWarshall petersenGraph))]
                             else []
            gis = maybe defaultGraphs (map (second rawGraphToGraphInfo)) $ _siGraphs si
        case gis of
          [] -> text "Unknown chain graph"
          _ -> mdo
            hoverChanges <- listWithKey (downHeightWithMax <$> dbt)
                                        (rowsWidget dti gis hoveredBlock maxNumChains)
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

--mkGrid n [] = []
--mkGrid n xs = let (x,rest) = splitAt n xs
--               in x : mkGrid n rest

downHeightWithMax :: BlockTable -> Map (Down (BlockHeight, BlockHeight)) (Map ChainId BlockHeaderTx)
downHeightWithMax bt = M.mapKeys (\a -> Down (a, fromMaybe a mh)) btm
  where
    btm = _blockTable_blocks bt
    mh = fst <$> M.lookupMax btm

chainDifficulty :: ChainId -> BlockTable -> Text
chainDifficulty cid bt =
  maybe "" (\b -> diffStr (blockDifficulty $ _blockHeaderTx_header b)) $
    M.lookup cid $ _blockTable_cut bt


rowsWidget
  :: (MonadAppIO r t m, Prerender js t m,
      HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> AllGraphs
  -> Dynamic t (Maybe BlockRef)
  -> Int
  -> Down (BlockHeight, BlockHeight)
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe BlockRef))
rowsWidget ti gis hoveredBlock maxNumChains (Down bh) cs = do
  hoverChanges <- blockHeightRow ti gis hoveredBlock maxNumChains (fst bh) cs
  spacerRow gis cs hoveredBlock maxNumChains bh
  return hoverChanges

blockHeightRow
  :: (MonadAppIO r t m, Prerender js t m,
      HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> AllGraphs
  -> Dynamic t (Maybe BlockRef)
  -> Int
  -> BlockHeight
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> m (Event t (Maybe BlockRef))
blockHeightRow ti gis hoveredBlock maxNumChains height headers = do
  divClass "block-row" $ do
    elClass "span" "block-height" $ text $ tshow height
    let chains = giChains $ getGraphAt height gis
        chainList = Set.toAscList chains
    es <- forM chainList $
      blockWidget0 ti gis hoveredBlock maxNumChains headers height

    -- Fill in the rest of the row with placeholder blocks
    replicateM_ (maxNumChains - Set.size chains) $ do
      elClass "span" "summary-details" $ divClass "empty-summary-inner" blank
    return $ (height,) <$$> leftmost es

blockWidget0
  :: (MonadAppIO r t m, Prerender js t m,
      HasJSContext (Performable m),
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Dynamic t TickInfo
  -> AllGraphs
  -> Dynamic t (Maybe BlockRef)
  -> Int
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> BlockHeight
  -> ChainId
  -> m (Event t (Maybe ChainId))
blockWidget0 ti gis hoveredBlock maxNumChains hs height cid = do
  net <- asks _as_network
  let mkAttrs = \case
        Nothing -> "class" =: ("blk" <> tshow cid <> " summary-details")
        Just hb -> if isDownstreamFrom gis hb (height, cid)
                     then "class" =: ("blk" <> tshow cid <> " summary-details hovered-block")
                     else "class" =: ("blk" <> tshow cid <> " summary-details")
      mkAttrs2 bh mhb = mkAttrs mhb <> sty
        where
          sty = case _blockHeaderTx_payload bh of
            Nothing -> mempty
            Just p ->
              case (_minerData_publicKeys $ _blockPayload_minerData p) of
                [] -> mempty
                (k:_) -> "style" =: ("background-color: #" <> T.take 3 k <> "4")
  let mbh = M.lookup cid <$> hs
  res <- viewIntoMaybe mbh (elClass "span" "summary-details" $ return never) $ \bh -> do
    (e,_) <- elDynAttr' "span" (mkAttrs2 <$> bh <*> hoveredBlock) $ do
      let getHeight = _blockHeader_height . _blockHeaderTx_header
      let mkRoute h = addNetRoute net (unChainId cid) $ Chain_BlockHeight :/ getHeight h :. Block_Header :/ () --TODO: Which NetId should it be?
      dynRouteLink (mkRoute <$> bh) $ divClass "summary-inner" $ do
        when (maxNumChains <= 10) $ el "div" $ do
          elClass "span" "block-hash" $ do
            dynText $ maybe "-" (T.take 4 . _minerData_account . _blockPayload_minerData) . _blockHeaderTx_payload <$> bh
            --dynText $ T.take 8 . hashHex . _blockHeader_hash . _blockHeaderTx_header <$> bh

        let getCreationTime = posixSecondsToUTCTime . _blockHeader_creationTime . _blockHeaderTx_header
        void $ prerender blank $ divClass "blockdiv" $ do
          let dt = Just . getCreationTime <$> bh
          let diffTimeToSecsAgo = diffTimeToRelativeEnglish maxNumChains
          let calcDiff lastTick t = maybe "" (diffTimeToSecsAgo . diffUTCTime (_tickInfo_lastUTC lastTick)) t
          dynText (calcDiff <$> ti <*> dt)

        divClass "blockdiv" $ do
          dynText $ maybe "" (\c -> tshow c <> " txs") . _blockHeaderTx_txCount <$> bh

        --divClass "blockdiv" $ do
        --  dynText $ diffStr . fromIntegral . targetToDifficulty . leToInteger .
        --            unBytesLE . _blockHeader_target . _blockHeaderTx_header <$> bh

    return $ leftmost [ fmap (const cid) <$> tag (current mbh) (domEvent Mouseenter e)
                      , Nothing <$ domEvent Mouseleave e]
  switch <$> hold never res

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

diffTimeToRelativeEnglish :: Int -> NominalDiffTime -> Text
diffTimeToRelativeEnglish numChains delta = tshow amt <> ptext
  where
    (amt, period) = diffTimeToRoundedQuantity delta
    ptext = if numChains <= 10
              then shortPeriodText period <> " ago"
              else shortPeriodText period

diffTimeToRoundedQuantity :: NominalDiffTime -> (Int, TimePeriod)
diffTimeToRoundedQuantity delta
  | delta < oneMinute * 2 = (roundInt delta, Seconds)
  | delta < oneHour = (roundInt $ delta / oneMinute, Minutes)
  | delta < oneDay = (roundInt $ delta / oneHour, Hours)
  | delta < oneWeek = (roundInt $ delta / oneDay, Days)
  | delta < oneYear = (roundInt $ delta / oneWeek, Weeks)
--  | delta < oneYear = (roundInt $ delta / oneMonth, Months)
  | otherwise = (roundInt $ delta / oneYear, Years)

data TimePeriod
  = Seconds
  | Minutes
  | Hours
  | Days
  | Weeks
--  | Months
  | Years

shortPeriodText :: TimePeriod -> Text
shortPeriodText = \case
  Seconds -> "s"
  Minutes -> "m"
  Hours -> "h"
  Days -> "d"
  Weeks -> "w"
--  Months -> "s"
  Years -> "y"

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

blockSeparation :: Int
blockSeparation = 50

spacerRow
  :: (MonadAppIO r t m, DomBuilder t m, PostBuild t m)
  => AllGraphs
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> Int
  -> (BlockHeight, BlockHeight)
  -> m ()
spacerRow gis cs hoveredBlock maxNumChains bhp = do
  let sty = "margin-left: 80px; height: " <> tshow blockSeparation <> "px; " <>
            "border: 0; padding: 0;"
  elAttr "div" ("class" =: "spacer-row" <>
                "style" =: sty ) $ do
    chainweb gis cs hoveredBlock maxNumChains (fst bhp)

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
  :: (MonadAppIO r t m, DomBuilder t m, PostBuild t m)
  => AllGraphs
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> Int
  -> BlockHeight
  -> m ()
chainweb gis cs hoveredBlock maxNumChains bh = do
  let chains = giChains $ getGraphAt bh gis
      totalWidth = 1100
      blockWidth = totalWidth `div` maxNumChains
      attrs = ("viewBox" =: ("0 0 " <> tshow totalWidth <> " " <> tshow (blockSeparation + 4)) <>
               "style" =: "vertical-align: middle;")
  svgElAttr "svg" attrs $ do
    forM_ chains (\c -> linksFromBlock gis blockWidth cs hoveredBlock (bh, c))
    void $ networkView $ lastLinesForActiveBlock gis blockWidth cs hoveredBlock bh <$> hoveredBlock

lastLinesForActiveBlock
  :: (DomBuilder t m, PostBuild t m)
  => AllGraphs
  -> Int
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockHeight
  -> Maybe BlockRef
  -> m ()
lastLinesForActiveBlock _ _ _ _ _ Nothing = blank
lastLinesForActiveBlock gis blockWidth cs hoveredBlock curBH (Just b) =
  if curBH > fst b then blank else linksFromBlock gis blockWidth cs hoveredBlock b

linksFromBlock
  :: (DomBuilder t m, PostBuild t m)
  => AllGraphs
  -> Int
  -> Dynamic t (Map ChainId BlockHeaderTx)
  -> Dynamic t (Maybe BlockRef)
  -> BlockRef
  -> m ()
linksFromBlock gis blockWidth cs hoveredBlock fromBlock = do
    let bh = fst fromBlock
    let fromChainId = snd fromBlock
    let fromChainInt = unChainId fromChainId
    let mkAttrs bs mhb = stroke <> (maybe ("style" =: "display: none;") mempty (M.lookup (snd fromBlock) bs))
          where
            stroke =
              case mhb of
                Nothing -> "stroke" =: "rgb(220,220,220)"
                Just hb ->
                  if bh > fst hb
                    then "stroke" =: "rgb(220,220,220)"
                    else if isDownstreamFrom gis hb fromBlock
                           then "stroke" =: "rgb(100,100,100)" <>
                                "stroke-width" =: "1.0"
                           else "stroke" =: "rgb(220,220,220)"
    svgElDynAttr "g" (mkAttrs <$> cs <*> hoveredBlock) $ do
      linkFromTo fromChainInt fromChainInt
      _ <- networkView $ ffor cs $ \cmap -> do
        case M.lookup fromChainId cmap of
          Nothing -> blank
          Just b -> mapM_ (linkFromTo fromChainInt . unChainId) $
            M.keys $ _blockHeader_neighbors $ _blockHeaderTx_header b
      return ()
  where
    linkFromTo :: (DomBuilder t m, PostBuild t m) => Int -> Int -> m ()
    linkFromTo f t =
        svgElAttr "line" ("x1" =: (tshow $ fromPos f) <>
                          "y1" =: "0" <>
                          "x2" =: (tshow $ toPos t) <>
                          "y2" =: (tshow (blockSeparation + 4)) ) blank

    fromPos :: Int -> Int
    fromPos f = blockWidth `div` 2 + f * blockWidth

    toPos :: Int -> Int
    toPos t = blockWidth `div` 2 + t * blockWidth
