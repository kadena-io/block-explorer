{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Frontend.Transfer where

------------------------------------------------------------------------------
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Foldable
import           Data.Scientific
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Data.Decimal
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as T
import           Data.Text.Read (decimal)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
import           Servant.Reflex
import           Text.Read
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           ChainwebData.AccountDetail
import           ChainwebData.Api
import           ChainwebData.EventDetail
import           ChainwebData.Pagination
import           Common.Route
import           Common.Types
import           Common.Utils
import           Frontend.Accounts
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Page.Block
import           Frontend.Transactions
import           PactNumber
------------------------------------------------------------------------------

transferSearchPage
  :: ( MonadApp r t m
     , Prerender js t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadIO m
     )
  => App [Text] t m ()
transferSearchPage = do
  r <- askRoute
  void $ networkView $ transferHelper <$> r

transferHelper
  :: ( MonadApp r t m
     , DomBuilder t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadIO m
     )
  => [Text]
  -> App [Text] t m ()
transferHelper (account:xs) = transferWidget account token chainid fromheight
  where
    token = fromMaybe "coin" $ xs ^? ix 0
    chainid = xs ^? ix 1 . to decimal . _Right . _1 -- TODO: If this doesn't parse, we should throw back an appropriate error
    fromheight = xs ^? ix 2 . to decimal . _Right . _1 -- TODO: If this doesn't parse, we should throw back an appropriate error
transferHelper _ = text "Invalid transfer url!"

transferWidget
  :: ( MonadApp r t m
     , DomBuilder t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadIO m
     )
  => Text
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> App [Text] t m ()
transferWidget account token chainid fromheight = do
  (AppState n si _ _) <- ask -- TODO: add chainweb-data host to appstate record
  let chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
      mkXhr lim off = transferXhr chainwebHost account token chainid fromheight lim off
  case mkXhr Nothing Nothing of
    Left e -> el "div" $ text $ "Error constructing XHR: " <> T.pack e
    Right xhr -> do
      pb <- getPostBuild
      result <- performRequestAsync $ xhr <$ pb
      maccs <- (\xhr -> xhr >>= _xhrResponse_responseText >>= getAccountDetail) <$$> holdDyn Nothing (Just <$> result)
      networkView $ flip fmap maccs $ \case
        Nothing -> inlineLoader "Loading..."
        Just accs -> do
          elAttr "h2" ("data-tooltip" =: account) $ text $ "Transfer Info"
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Account" $ accountSearchLink n token account account
              tfield "Token" $ text token
              maybe (pure ()) (\cid -> tfield "Chain ID" $ text $ tshow cid) chainid
          elClass "table" "ui celled table" $ do
            el "thead" $ el "tr" $ do
              el "th" $ text "Request Key"
              maybe (el "th" $ text "Chain ID") (const $ pure ()) chainid
              el "th" $ text "Block Height"
              el "th" $ text "From/To"
              el "th" $ text "Amount"
            el "tbody" $ do
              forM_ accs $ \acc -> el "tr" $ do
                let hash = _acDetail_blockHash acc
                    requestKey = _acDetail_requestKey acc
                    cid = _acDetail_chainid acc
                    height = _acDetail_height acc
                    idx = _acDetail_idx acc
                    fromAccount = _acDetail_fromAccount acc
                    toAccount = _acDetail_toAccount acc
                    amount = _acDetail_amount acc
                elAttr "td" ("class" =: "cut-text" <> "data-label" =: "Request Key" <> "data-tooltip" =: requestKey) $
                  txDetailLink n requestKey requestKey
                when (isNothing chainid) $ elAttr "td" ("data-label" =: "Chain ID") $ text $ tshow cid
                elAttr "td" ("data-label" =: "Block Height" <> "data-tooltip" =: hash) $
                  blockHashLink n (ChainId $ fromIntegral cid) hash (tshow height)
                let showAccount = listToMaybe [a | a <- [fromAccount, toAccount], a /= account, not $ T.null a]
                elAttr "td" ("class" =: "cut-text" <> "data-label" =: "From/To" <> foldMap (\s -> "data-tooltip" =: s) showAccount) $
                  maybe (pure ()) (\s -> accountSearchLink n token s s) showAccount
                let isNegAmt = fromAccount == account
                elAttr "td" ("data-label" =: "Amount" <> "style" =: if isNegAmt then "color: red" else "color: green") $ do
                  let printedAmount amt = formatScientific Fixed Nothing amt
                  text $ T.pack $ if isNegAmt then printedAmount (negate amount) else printedAmount amount
      pure ()

mkTransferSearchRoute :: NetId -> Text -> Text -> R FrontendRoute
mkTransferSearchRoute netId account token = mkNetRoute netId (NetRoute_TransferSearch :/ [account,token])

getAccountDetail :: Text -> Maybe [AccountDetail]
getAccountDetail = A.decode . T.encodeUtf8 . fromStrict
