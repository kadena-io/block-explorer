{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Frontend.Page.ReqKey where

------------------------------------------------------------------------------
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T (pack)
import Data.Word
import GHC.Generics (Generic)
import GHCJS.DOM.Types (MonadJSM)
import Obelisk.Route
import Obelisk.Route.Frontend
import Pact.Types.API
import Reflex.Dom.Core hiding (Value)
import Reflex.Network
import Text.Printf (printf)
------------------------------------------------------------------------------
import Chainweb.Api.ChainId
import Common.Route
import Common.Types
import Common.Utils
import Frontend.App
import Frontend.AppState
import Frontend.ChainwebApi
import Frontend.Common
------------------------------------------------------------------------------

-- | Intermediate form for the results of /poll endpoint
-- queries
--
data PollResult = PollResult
  { _cr_reqKey :: Text
  , _cr_txId :: Word64
  , _cr_result :: Value
  , _cr_gas :: Int
  , _cr_logs :: Text
  , _cr_continuation :: Value
  , _cr_metaData :: Maybe Value
  } deriving (Eq,Show,Generic)

instance FromJSON PollResult where
  parseJSON = withObject "PollResult" $ \o -> PollResult
    <$> o .: "reqKey"
    <*> o .: "txId"
    <*> o .: "result"
    <*> o .: "gas"
    <*> o .: "logs"
    <*> o .: "continuation"
    <*> o .:? "metaData"

requestKeyWidget
    :: (MonadApp r t m, MonadJSM (Performable m), HasJSContext (Performable m),
       RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Monad (Client m))
    => ServerInfo
    -> NetId
    -> Int
    -> App Text t m ()
requestKeyWidget si _netId cid = do
    as <- ask
    let n = _as_network as
        chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
        c = ChainId cid

    reqKey <- askRoute

    cmdResult :: Event t (Maybe Value) <- fromRequestKey chainwebHost c reqKey
    void $ networkHold (inlineLoader "Retrieving command result...") $ ffor cmdResult $ \case
      Nothing -> dynText (nothingMessage <$> reqKey)
      Just v -> case v of
        Object o ->
          if HM.null o
          then dynText (reqKeyMessage <$> reqKey)
          else text $ tshow o
        _ -> dynText (aesonMessage <$> reqKey)
     -- case fromJSON v of
     --   Success pr -> requestKeyResultPage pr
     --   _ -> dynText (reqKeyMessage <$> reqKey)

  where
    nothingMessage s =
      T.pack $ printf "Unknown error returned while polling for request key " <> (show s)
    aesonMessage s =
      T.pack $ "Unexpected result returned while polling for request key " <> show s
    reqKeyMessage s =
      T.pack $ printf "Your request key %s is not associated with an already processed transaction." (show s)

requestKeyResultPage
    :: ( MonadApp r t m
       , MonadJSM (Performable m)
       , HasJSContext (Performable m)
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , Monad (Client m)
       )
    => PollResult
    -> App Text t m ()
requestKeyResultPage (PollResult rk txid pr g logs pcont meta) = do
    el "h2" $ text "Transaction"
    elAttr "table" ("class" =: "ui definition table") $ do
      el "tbody" $ do
        tfield "Request Key" $ text $ tshow rk
        tfield "Transaction Id" $ text $ tshow txid
        tfield "Result" $ text $ tshow pr
        tfield "Gas" $ text $ tshow g
        tfield "Logs" $ text $ tshow logs
        tfield "Continuation" $ text $ tshow pcont
        tfield "Metadata" $ text $ tshow meta
