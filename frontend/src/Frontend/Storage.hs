{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Storage
  ( localStorage
  , sessionStorage
  , getItemStorage
  , setItemStorage
  --, removeItemStorage
  , Storage (..)
  , StoreType (..)
  , browserStorage
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHCJS.DOM.Types (JSString, fromJSString, toJSString)
import Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Storage as GHCJS
import qualified GHCJS.DOM.Window as Window

data StoreType
  = StoreType_Local
  | StoreType_Session
  deriving (Eq, Show)

-- | Local/session storage manager. On mac, this should store the data _outside_
-- the browser storage, such that data is persisted.
data Storage = Storage
  { _storage_get
    :: forall k a. (Show (k a), FromJSON a)
    => StoreType -> k a -> JSM (Maybe a)
  -- ^ Get an item using the key `k a`
  , _storage_set
    :: forall k a. (Show (k a), ToJSON a)
    => StoreType -> k a -> a -> JSM ()
  -- ^ Set the item at key `k a` to the value `a`
  , _storage_remove
    :: forall k a. Show (k a)
    => StoreType -> k a -> JSM ()
  -- ^ Remove an item using the key `k a`
  }

getItemStorage
  :: (MonadJSM m, Show (k a), Aeson.FromJSON a)
  => Storage -> StoreType -> k a -> m (Maybe a)
getItemStorage s st k = liftJSM $ _storage_get s st k

setItemStorage
  :: (MonadJSM m, Show (k a), Aeson.ToJSON a)
  => Storage -> StoreType -> k a -> a -> m ()
setItemStorage s st k a = liftJSM $ _storage_set s st k a

--removeItemStorage
--  :: (MonadJSM m, HasStorage m, Show (k a))
--  => StoreType -> k a -> m ()
--removeItemStorage s k = liftJSM . (\c -> _storage_remove c s k) =<< askStorage

-- | Uses the browser's local/session storage, as appropriate
browserStorage :: Storage
browserStorage = Storage
  { _storage_get = \storeType key -> do
    storage <- getStorage storeType
    (fromJsonString =<< ) <$> GHCJS.getItem storage (keyToString key)
  , _storage_set = \storeType key data' -> do
    storage <- getStorage storeType
    GHCJS.setItem storage (keyToString key) (toJsonString data')
  , _storage_remove = \storeType key -> do
    storage <- getStorage storeType
    GHCJS.removeItem storage (keyToString key)
  }
  where
    getStorage = \case
      StoreType_Local -> Window.getLocalStorage =<< DOM.currentWindowUnchecked
      StoreType_Session -> Window.getSessionStorage =<< DOM.currentWindowUnchecked
    toJsonString :: ToJSON a => a -> JSString
    toJsonString = toJSString . T.decodeUtf8With T.lenientDecode . BL.toStrict . Aeson.encode
    fromJsonString :: FromJSON a => JSString -> Maybe a
    fromJsonString = Aeson.decodeStrict . T.encodeUtf8 . fromJSString
    keyToString :: Show a => a -> JSString
    keyToString = toJSString . T.pack . show

-- | Get access to browser's local storage.
localStorage :: StoreType
localStorage = StoreType_Local

-- | Get access to browser's session storage.
sessionStorage :: StoreType
sessionStorage = StoreType_Session
