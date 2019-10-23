{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend.AppState where

------------------------------------------------------------------------------
--import           Control.Error
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics
import           GHCJS.DOM.Types (MonadJSM)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.EventSource
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Utils
import           Frontend.ChainwebApi
------------------------------------------------------------------------------

-- TODO Move into common later
type Batch a = [a]

batchOne :: a -> Batch a
batchOne a = [a]

data AppTriggers = AppTriggers
  deriving Generic

instance Semigroup AppTriggers where
  AppTriggers <> AppTriggers = AppTriggers

instance Monoid AppTriggers where
    mempty = AppTriggers
    mappend = (<>)

makeLenses ''AppTriggers

trigger
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t a
    -> m ()
trigger l e = triggerBatch l $ batchOne <$> e

triggerBatch
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t (Batch a)
    -> m ()
triggerBatch l e = tellEvent $ (\as -> set l as mempty) <$> e

data HashrateData = HashrateData
    { _hd_deltaT :: NominalDiffTime
    , _hd_difficulty :: Integer
    } deriving (Eq,Ord,Show)

data GlobalStats = GlobalStats
    { _gs_txCount :: Word64
    , _gs_startTime :: UTCTime
    , _gs_hashrates :: Map ChainId HashrateData
    } deriving (Eq,Ord,Show)

-- Not used right now because calculating it over the whole blockTable is
-- more stable.
--calcHashrate :: HashrateData -> Double
--calcHashrate (HashrateData dt diff) = fromIntegral diff / realToFrac dt
--
--calcNetworkHashrate :: GlobalStats -> Maybe Double
--calcNetworkHashrate gs =
--    if null hrs
--      then Nothing
--      else Just (10 * sum hrs / fromIntegral (length hrs))
--  where
--    hrs = map calcHashrate $ M.elems (_gs_hashrates gs)

calcNetworkHashrate :: POSIXTime -> BlockTable -> Maybe Double
calcNetworkHashrate now bt =
    if now - earliestTime < 1
      then Nothing
      else Just (totalDifficulty / (realToFrac $ now - earliestTime))
  where
    (earliestTime, totalDifficulty) = M.foldl' f (now,0) (_blockTable_blocks bt)
    f (et, td) next =
      let (etNew, tdNew) = M.foldl' g (now,0) next
       in (min et etNew, td + tdNew)
    g (et, td) next =
      (min et (_blockHeader_creationTime $ _blockHeaderTx_header next),
       td + blockDifficulty (_blockHeaderTx_header next))

getNewHashrateData
  :: BlockTable
  -> BlockHeaderTx
  -> Either String (ChainId, HashrateData)
getNewHashrateData bt bhtx = do
    bhtxPrev <- note "Error getting previous block" $ getBlock height cid bt

    -- We calculate difficulty based on the target because this results
    -- in a smoother aggregate difficulty / hashrate estimate.
    let difficulty = targetToDifficulty $ leToInteger $ unBytesLE $ _blockHeader_target $ _blockHeaderTx_header bhtx
    let hrd = HashrateData (delta bhtxPrev bhtx) difficulty
    return (cid, hrd)
  where
    hdr = _blockHeaderTx_header bhtx
    cid = _blockHeader_chainId hdr
    height = _blockHeader_height hdr - 1
    getTime = _blockHeader_creationTime . _blockHeaderTx_header
    delta b0 b1 = getTime b1 - getTime b0

addHashrateData :: (ChainId, HashrateData) -> GlobalStats -> GlobalStats
addHashrateData (cid, hrd) gs = gs { _gs_hashrates = M.insert cid hrd hrs }
  where
    hrs = _gs_hashrates gs



addTxCount :: BlockHeaderTx -> GlobalStats -> GlobalStats
addTxCount bhtx gs = gs { _gs_txCount = _gs_txCount gs + maybe 0 fromIntegral (_blockHeaderTx_txCount bhtx) }

setStartTime :: UTCTime -> GlobalStats -> GlobalStats
setStartTime t gs = gs { _gs_startTime = t }

data AppState t = AppState
    { _as_host :: ChainwebHost
    , _as_serverInfo :: ServerInfo
    , _as_blockTable :: Dynamic t BlockTable
    , _as_stats :: Dynamic t GlobalStats
    } deriving Generic

getMissing :: BlockTable -> BlockHeaderTx -> [(ChainId, Hash)]
getMissing bt bhtx = filter (\p -> getBlock (height-1) (fst p) bt == Nothing) cids
  where
    h = _blockHeaderTx_header bhtx
    height = _blockHeader_height h
    cids = (_blockHeader_chainId h, _blockHeader_parent h) :
           M.toList (_blockHeader_neighbors h)


stateManager
    :: (DomBuilder t m, MonadHold t m, Prerender js t m, MonadFix m,
        PostBuild t m, MonadJSM (Performable m), HasJSContext (Performable m),
        PerformEvent t m, TriggerEvent t m)
    => Text
    -- ^ Application route...not in use yet
    -> ChainwebHost
    -> ServerInfo
    -> Event t AppTriggers
    -- ^ Not in use yet
    -> m (AppState t)
stateManager _ h si _ = do
    let cfg = EventSourceConfig never True
    es <- startEventSource h cfg
    let downEvent = _eventSource_recv es

    ebt <- getBlockTable h si

    rec blockTable <- foldDyn ($) mempty $ mergeWith (.)
          [ (<>) <$> ebt
          , (\mbtx bt -> maybe bt (insertBlockTable bt) mbtx) <$> downEvent
          , (\pair bt -> maybe bt (insertBlockTable bt . pairToBhtx) pair) <$> newMissing
          ]

        let missingBlocks = getMissingBlocks blockTable downEvent
            getHeader (cid,hash) = getBlockHeader h cid (hashB64U hash)
            eme = sequence . fmap getHeader <$> missingBlocks
        ee <- networkHold (return []) eme
        let newMissing = switch (current (leftmost <$> ee))


    let newHrd = attachWith getNewHashrateData (current blockTable) $ fmapMaybe id downEvent

    pb <- getPostBuild
    now <- prerender (return t0) (liftIO getCurrentTime)
    stats <- foldDyn ($) (GlobalStats 0 t0 mempty) $ mergeWith (.)
      [ maybe id addTxCount <$> downEvent
      , setStartTime <$> tag (current now) pb
      , addHashrateData <$> filterRight newHrd
      ]

    return $ AppState h si blockTable stats
  where
    t0 = UTCTime (ModifiedJulianDay 0) 0

pairToBhtx :: (BlockHeader, Text) -> BlockHeaderTx
pairToBhtx (h, bhBinBase64) =
    BlockHeaderTx h Nothing mPowHash Nothing
  where
    mPowHash = either (const Nothing) Just (calcPowHash =<< decodeB64UrlNoPaddingText bhBinBase64)

getMissingBlocks
  :: Reflex t
  => Dynamic t BlockTable
  -> Event t (Maybe BlockHeaderTx)
  -> Event t [(ChainId, Hash)]
getMissingBlocks blockTable downEvent = ffilter (not . null) $
  attachWith getMissing (current blockTable) (fmapMaybe id downEvent)

startEventSource
  :: (DomBuilder t m, Prerender js t m)
  => ChainwebHost
  -> EventSourceConfig t
  -> m (RawEventSource t (Maybe BlockHeaderTx))
startEventSource h esCfg = do
  res <- prerender (pure neverEventSource) $ do
    RawEventSource r o e <- jsonEventSource (headerUpdatesUrl h) "BlockHeader" esCfg
    return (RawEventSource (fmapMaybe id r) o e)
  let r = switch $ current $ _eventSource_recv <$> res
  let o = switch $ current $ _eventSource_open <$> res
  let e = switch $ current $ _eventSource_error <$> res
  return $ RawEventSource r o e

neverEventSource :: Reflex t => RawEventSource t a
neverEventSource = RawEventSource never never never
