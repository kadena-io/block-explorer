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
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Strict as SM
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word
import           GHC.Generics
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.EventSource
------------------------------------------------------------------------------
import           Chainweb.Api.Base64Url
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BlockHeaderTx
import           Chainweb.Api.BytesLE
import           Chainweb.Api.ChainId
import           Chainweb.Api.Hash
import           Common.Types
import           Common.Utils
import           Frontend.ChainwebApi
import           Frontend.Common
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
    { _hd_deltaT     :: NominalDiffTime
    , _hd_difficulty :: Integer
    } deriving (Eq,Ord,Show)

data GlobalStats = GlobalStats
    { _gs_txCount :: Int
    , _gs_startTime :: UTCTime
    , _gs_hashrates :: Map ChainId HashrateData
    , _gs_moduleCount :: Word64
    , _gs_totalTxCount :: Maybe Int
    , _gs_circulatingCoins :: Maybe Double
    , _gs_possibleCoins :: Maybe Double
    } deriving (Eq,Ord,Show)

makeLenses ''GlobalStats

calcNetworkHashrate :: POSIXTime -> BlockTable -> Maybe Double
calcNetworkHashrate curTime bt =
    if curTime - earliestTime < 1
      then Nothing
      else Just (totalDifficulty / (realToFrac $ curTime - earliestTime))
  where
    (earliestTime, totalDifficulty) = M.foldl' f (curTime,0) (_blockTable_blocks bt)
    f (et, td) next =
      let (etNew, tdNew) = M.foldl' g (curTime,0) next
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
addTxCount bhtx gs = gs
    & gs_txCount +~ newCount
    & gs_totalTxCount . _Just +~ newCount
  where
    newCount = (maybe 0 fromIntegral (_blockHeaderTx_txCount bhtx))

addModuleCount :: BlockHeaderTx -> GlobalStats -> GlobalStats
addModuleCount bhtx gs = gs { _gs_moduleCount = _gs_moduleCount gs + moduleCount }
  where
    moduleCount = 0
    _txs = _blockHeaderTx_payload bhtx

setStartTime :: UTCTime -> GlobalStats -> GlobalStats
setStartTime t gs = gs { _gs_startTime = t }

data AppState t = AppState
    { _as_network    :: NetId
    , _as_serverInfo :: ServerInfo
    , _as_netConfig :: Maybe NetConfig
    , _as_graphAdjacencies :: SM.Map Int Int
    } deriving Generic

getMissing :: BlockTable -> BlockHeaderTx -> [(ChainId, Hash)]
getMissing bt bhtx = filter (\p -> getBlock (height-1) (fst p) bt == Nothing) cids
  where
    h = _blockHeaderTx_header bhtx
    height = _blockHeader_height h
    cids = (_blockHeader_chainId h, _blockHeader_parent h) :
           M.toList (_blockHeader_neighbors h)

launchTime :: UTCTime
launchTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" "2020-01-15T00:00:00"

stateManager
    :: DomBuilder t m
    => Text
    -- ^ Application route...not in use yet
    -> DataBackends
    -> NetId
    -> ServerInfo
    -> Event t AppTriggers
    -- ^ Not in use yet
    -> m (AppState t)
stateManager _ (DataBackends ndbs) n si _ = do
    let adjs = maybe mempty (SM.fromList . zip [0..] . floydWarshall . M.fromList . snd . head) $ _siGraphs si
    return $ AppState n si (M.lookup (_siChainwebVer si) ndbs) adjs

pairToBhtx :: (BlockHeader, Text) -> BlockHeaderTx
pairToBhtx (h, bhBinBase64) =
    BlockHeaderTx h Nothing mPowHash Nothing Nothing
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
