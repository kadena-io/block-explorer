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
import           Data.Foldable
import           Data.Functor
import           Data.Scientific
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe hiding (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as T
import           Data.Text.Read (decimal)
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           ChainwebData.AccountDetail
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
      maccs <- (\xhr' -> (xhr' >>= _xhrResponse_responseText >>= getAccountDetail >>= pure . (,foldMap _xhrResponse_headers xhr'))) <$$> holdDyn Nothing (Just <$> result)
      _ <- networkView $ flip fmap maccs $ \case
        Nothing -> inlineLoader "Loading..." 
        Just (accs,headers) -> mdo
          elAttr "h2" ("data-tooltip" =: account) $ text $ "Transfer Info"
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Token" $ text token
              tfield "Account" $ accountSearchLink n token account account
              maybe (pure ()) (\cid -> tfield "Chain ID" $ text $ tshow cid) chainid
          elAttr "div" ("style" =: "display: grid") $ mdo
            t <- elClass "table" "ui celled table" $ do
              el "thead" $ el "tr" $ do
                el "th" $ text "Request Key"
                maybe (el "th" $ text "Chain ID") (const $ pure ()) chainid
                el "th" $ text "Block Height"
                el "th" $ text "From/To"
                el "th" $ text "Amount"
              el "tbody" $ do
                forM_ accs $ \acc -> el "tr" $ drawRow n token account chainid acc
                p <- produceNewRowsOnToken mkXhr n token account chainid e
                let (render,newToken) = splitE p
                indexWithRender <- accum (\(key,_oldRender) newRender -> (succ key, newRender)) (0 :: Integer, pure ()) render
                void $ listHoldWithKey mempty (indexWithRender <&> \(i,r) -> M.singleton i (Just r)) (\_ r -> r)
                let dumpEmpty = \case
                      Just "" -> Nothing
                      Just tt -> Just tt
                      Nothing -> Nothing
                return $ fmap dumpEmpty newToken
            e <- case M.lookup "Chainweb-Next" headers of
              Nothing -> pure never
              Just next -> evaporateButtonOnClick next t
            pure ()

      pure ()

nextButtonText :: Text
nextButtonText = "Fetch more results..."


fetchButton :: DomBuilder t m => Text -> m (Event t ())
fetchButton s = do
  let ourAttrs = mconcat [ "type" =: "button", "style" =: "margin: auto" ]
  (e, _) <- elAttr' "button" ourAttrs $ text s
  return $ domEvent Click e

evaporateButtonOnClick
  :: MonadFix m
  => Reflex t
  => DomBuilder t m
  => MonadHold t m
  => PostBuild t m
  => Text -> Event t (Maybe Text) -> m (Event t Text)
evaporateButtonOnClick token t = mdo
  let something = leftmost [(inlineLoader "Fetching more rows..." >> pure never) <$ click, v]
      v = t <&> \case 
        Just newToken -> (const newToken) <$$> fetchButton nextButtonText
        Nothing -> pure never
  beenClicked <- holdDyn ((const token) <$$> fetchButton nextButtonText) something
  clickNow <- networkView beenClicked
  click <- switchHold never clickNow
  pure click

type TransferXHR = Maybe Int -> Maybe Int -> Maybe Text -> Either String (XhrRequest ())

produceNewRowsOnToken 
  :: (MonadApp r t m, MonadJSM (Performable m),
  HasJSContext (Performable m), Prerender js t m,
  RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, MonadIO m)
  => TransferXHR -> NetId -> Text -> Text -> Maybe Int -> Event t Text -> m (Event t (m (), Maybe Text))
produceNewRowsOnToken mkXhr n token account chainid nextToken = do
  result <- performRequestAsync $ fmap (\t -> either error id $ mkXhr Nothing Nothing (Just t)) nextToken -- TODO: Don't use error here
  return $ mapMaybe id $ result <&> \xhr -> do
       r <- _xhrResponse_responseText xhr
       details <- getAccountDetail r
       let headers = _xhrResponse_headers xhr
       let rowsToRender = forM_ details $ \detail -> el "tr" $ drawRow n token account chainid detail
       return (rowsToRender, M.lookup "Chainweb-Next" headers)

drawRow 
  :: Monad m 
   => RouteToUrl (R FrontendRoute) m
   => SetRoute t (R FrontendRoute) m
   => DomBuilder t m
   => Prerender js t m
   => NetId -> Text -> Text -> Maybe Int -> AccountDetail -> m ()
drawRow n token account chainid acc = do
  let hash = _acDetail_blockHash acc
      requestKey = _acDetail_requestKey acc
      cid = _acDetail_chainid acc
      height = _acDetail_height acc
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
