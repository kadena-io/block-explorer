{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Frontend.Transfer where

------------------------------------------------------------------------------
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Foldable
import           Data.Functor
import           Data.Scientific
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Data.CaseInsensitive
import           Data.Decimal
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe hiding (mapMaybe)
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
import qualified Streaming.Prelude as SP
import qualified Streaming as SS
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
     , Reflex t
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
      mkXhr lim off nextToken = transferXhr chainwebHost account token chainid fromheight lim off nextToken
  case mkXhr Nothing Nothing Nothing of
    Left e -> el "div" $ text $ "Error constructing XHR: " <> T.pack e
    Right xhr -> mdo
      pb <- getPostBuild
      result <- performRequestAsync $ xhr <$ pb
      maccs <- (\xhr -> (xhr >>= _xhrResponse_responseText >>= getAccountDetail >>= pure . (,foldMap _xhrResponse_headers xhr))) <$$> holdDyn Nothing (Just <$> result)
      networkView $ flip fmap maccs $ \case
        Nothing -> inlineLoader "Loading..." 
        Just (accs,headers) -> mdo
          elAttr "h2" ("data-tooltip" =: account) $ text $ "Transfer Info"
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Account" $ accountSearchLink n token account account
              tfield "Token" $ text token
              maybe (pure ()) (\cid -> tfield "Chain ID" $ text $ tshow cid) chainid
          t <- elClass "table" "ui fixed table" $ do
            el "thead" $ el "tr" $ do
              el "th" $ text "Request Key"
              maybe (el "th" $ text "Chain ID") (const $ pure ()) chainid
              el "th" $ text "Block Height"
              el "th" $ text "From/To"
              el "th" $ text "Amount"
            el "tbody" $ do
              forM_ accs $ \acc -> el "tr" $ drawRow n token account chainid acc headers
              produceNewRowsOnToken mkXhr n token account chainid e
          e <- case M.lookup "Chainweb-Next" headers of
            Nothing -> pure never
            Just next -> evaporateButtonOnClick next t
          pure ()

      pure ()

evaporateButtonOnClick token t = mdo
  let something = leftmost [pure never <$ click, t <&> \newToken -> (const newToken) <$$> button newToken]
  beenClicked <- holdDyn ((const token) <$$> button token) something
  clickNow <- networkView beenClicked
  click <- switchHold never clickNow
  pure click
  

type TransferXHR = Maybe Int -> Maybe Int -> Maybe Text -> Either String (XhrRequest ())

produceNewRowsOnToken 
  :: (MonadApp r t m, MonadJSM (Performable m),
  HasJSContext (Performable m), Prerender js t m,
  RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m,
  -- MonadIO m) => TransferXHR -> NetId -> Text -> Text -> Maybe Int -> Event t Text -> m (Event t (Event t Text))
  MonadIO m) => TransferXHR -> NetId -> Text -> Text -> Maybe Int -> Event t Text -> m (Event t Text)
produceNewRowsOnToken mkXhr n token account chainid nextToken = do
  result <- performRequestAsync $ fmap (\token -> either error id $ mkXhr Nothing Nothing (Just token)) nextToken -- TODO: Don't use error here
  maccs <- (\xhr -> (xhr >>= _xhrResponse_responseText >>= getAccountDetail >>= pure . (,foldMap _xhrResponse_headers xhr))) <$$> holdDyn Nothing (Just <$> result)

  fmap (mapMaybe id) $ networkView $ flip fmap maccs $ \case
    Nothing -> pure Nothing
    Just (accs,headers) -> do
      forM_ accs $ \acc -> el "tr" $ drawRow n token account chainid acc headers
      case M.lookup "Chainweb-Next" headers of
        Nothing -> pure $ Just "End of stream!"
        Just next -> pure $ Just next

drawRow 
  :: Monad m 
   => RouteToUrl (R FrontendRoute) m
   => SetRoute t (R FrontendRoute) m
   => DomBuilder t m
   => Prerender js t m
   => NetId -> Text -> Text -> Maybe Int -> AccountDetail -> Map (CI Text) Text -> m ()
drawRow n token account chainid acc headers = do
  let hash = _acDetail_blockHash acc
      requestKey = _acDetail_requestKey acc
      cid = _acDetail_chainid acc
      height = _acDetail_height acc
      idx = _acDetail_idx acc
      fromAccount = _acDetail_fromAccount acc
      toAccount = _acDetail_toAccount acc
      amount = _acDetail_amount acc
  elAttr "td" ("class" =: "cut-text" <> "data-label" =: "Request Key" <> "data-tooltip" =: requestKey) $
    if requestKey == "<coinbase>" then text "coinbase" else txDetailLink n requestKey requestKey
  when (isNothing chainid) $ elAttr "td" ("data-label" =: "Chain ID") $ text $ tshow cid
  elAttr "td" ("data-label" =: "Block Height" <> "data-tooltip" =: hash) $
    blockHashLink n (ChainId $ fromIntegral cid) hash (tshow height)
  let showAccount = listToMaybe [a | a <- [fromAccount, toAccount], a /= account, not $ T.null a]
  elAttr "td" ("class" =: "cut-text" <> "data-label" =: "From/To" <> foldMap (\s -> "data-tooltip" =: s) showAccount) $
    case showAccount of
      Nothing -> pure ()
      Just s -> do
        text $ if s == fromAccount then "From: " else "To: "
        accountSearchLink n token s s
  let isNegAmt = fromAccount == account
  elAttr "td" ("data-label" =: "Amount" <> "style" =: if isNegAmt then "color: red" else "color: green") $ do
    let printedAmount amt = formatScientific Fixed Nothing amt
    text $ T.pack $ if isNegAmt then printedAmount (negate amount) else printedAmount amount

mkTransferSearchRoute :: NetId -> Text -> Text -> R FrontendRoute
mkTransferSearchRoute netId account token = mkNetRoute netId (NetRoute_TransferSearch :/ [account,token])

getAccountDetail :: Text -> Maybe [AccountDetail]
getAccountDetail = A.decode . T.encodeUtf8 . fromStrict
