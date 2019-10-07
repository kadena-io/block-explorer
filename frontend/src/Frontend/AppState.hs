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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend.AppState where

------------------------------------------------------------------------------
--import           Control.Error
import           Control.Lens
import           Control.Monad.Fix
import           Data.Text (Text)
import           GHC.Generics
import           GHCJS.DOM.Types (MonadJSM)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.EventSource
------------------------------------------------------------------------------
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

data AppState t = AppState
    { _as_host :: ChainwebHost
    , _as_serverInfo :: ServerInfo
    , _as_blockTable :: Dynamic t BlockTable
    } deriving Generic

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

    blockTable <- foldDyn ($) mempty $ leftmost
      [ (<>) <$> ebt
      , (\mbtx bt -> maybe bt (insertBlockTable bt) mbtx) <$> downEvent
      ]

    return $ AppState h si blockTable

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
