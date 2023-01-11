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
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe hiding (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as T
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
import           Servant.Common.Req hiding (note)
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           ChainwebData.AccountDetail
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
  => App AccountParams t m ()
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
  => AccountParams
  -> App r t m ()
transferHelper ap = do
     (AppState _ _ cw _) <- ask
     case cw >>= _netConfig_dataHost of
       Nothing -> text "Transfers view is not supported unless a chainweb-data url is included in the config!"
       Just _v -> transferWidget (apAccount ap) (apToken ap) (apChain ap) (fromJust cw) Nothing -- We can assume the netconfig exists if we got to this branch!

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
  -> Maybe Integer
  -> NetConfig
  -> Maybe Int
  -> App r t m ()
transferWidget account token chainid nc _fromheight = do
    pb <- getPostBuild
    res <- mkXhr QNone QNone (constDyn QNone) pb
    void $ networkHold (inlineLoader "Loading...") (f <$> res)
  where
    mkXhr lim off nextToken evt =
      getTransfers
      nc
      (constDyn lim)
      (constDyn off)
      (constDyn $ Right account)
      (constDyn $ QParamSome token)
      (constDyn $ maybe QNone (QParamSome . ChainId . fromIntegral) chainid)
      nextToken
        evt
    f = \case
        Left _ -> pure ()
        Right (accs, nextHeaderToken) -> mdo
          (AppState n _ _ _) <- ask
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
                let rowsToRender details = forM_ details $ \detail -> el "tr" $ drawRow n token account chainid detail
                rowsToRender accs
                p <- produceNewRowsOnToken mkXhr e
                let (errorE, goodE) = fanEither p
                let (details,newToken) = splitE goodE
                indexWithRender <- accum (\(key,_oldDetails) newDetails -> (succ key, rowsToRender newDetails)) (0 :: Integer, pure ()) details
                void $ listHoldWithKey mempty (indexWithRender <&> \(i,r) -> M.singleton i (Just r)) (\_ r -> r)
                return $ leftmost [fmap Left errorE, fmap Right newToken]
            e <- case nextHeaderToken of
              Nothing -> pure never
              Just (NextToken next) -> evaporateButtonOnClick next t
            pure ()


nextButtonText :: Text
nextButtonText = "Fetch more results..."


fetchButton :: DomBuilder t m => Text -> m (Event t ())
fetchButton s = do
  let ourAttrs = mconcat [ "type" =: "button", "class" =: "ui blue button", "style" =: "margin: auto" ]
  (e, _) <- elAttr' "button" ourAttrs $ text s
  return $ domEvent Click e

disabledButton :: DomBuilder t m => Text -> m (Event t ())
disabledButton s = do
  let ourAttrs = mconcat [ "type" =: "button", "class" =: "ui disabled button", "style" =: "margin: auto" ]
  (e,_) <- elAttr' "button" ourAttrs $ text s
  return $ domEvent Click e

evaporateButtonOnClick
  :: MonadFix m
  => Reflex t
  => DomBuilder t m
  => MonadHold t m
  => PostBuild t m
  => Text -> Event t (Either TransferError (Maybe Text)) -> m (Event t Text)
evaporateButtonOnClick token t = mdo
  let something = leftmost [(inlineLoader "Fetching more rows..." >> pure never) <$ click, v]
      v = t <&> \case
        Right (Just newToken) -> (const newToken) <$$> fetchButton nextButtonText
        Right Nothing -> pure never
        Left (NonHTTP200 status) -> disabledButton ("Non 200 HTTP Status: " <> T.pack (show status)) >> pure never
        Left (BadResponse txt) -> disabledButton ("Bad response: " <> txt) >> pure never
        Left (ReqFailure txt) -> disabledButton ("Request Failure: " <> txt) >> pure never
  beenClicked <- holdDyn ((const token) <$$> fetchButton nextButtonText) something
  clickNow <- networkView beenClicked
  click <- switchHold never clickNow
  pure click

type TransferRequest t m =
        QParam Limit
        -> QParam Offset
        -> Dynamic t (QParam NextToken)
        -> Event t ()
        -> m (Event t (Either TransferError ([AccountDetail], Maybe NextToken)))

produceNewRowsOnToken
  :: (MonadApp r t m, MonadJSM (Performable m),
  HasJSContext (Performable m), Prerender js t m,
  RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, MonadIO m)
  => TransferRequest t m -> Event t Text -> m (Event t (Either TransferError ([AccountDetail], Maybe Text)))
produceNewRowsOnToken mkXhr nextToken = do
    newTokenDyn <- holdDyn QNone (QParamSome . NextToken <$> nextToken)
    mkXhr QNone QNone newTokenDyn (() <$ nextToken) <&&> \case
            Left err -> Left err
            Right (details, nextTokenHeader) -> Right (details, unNextToken <$> nextTokenHeader)
  where
    (<&&>) = flip (fmap . fmap)


drawRow
  :: Monad m
   => RouteToUrl (R FrontendRoute) m
   => SetRoute t (R FrontendRoute) m
   => DomBuilder t m
   => Prerender js t m
   => NetId -> Text -> Text -> Maybe Integer -> AccountDetail -> m ()
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
mkTransferSearchRoute netId account token = mkNetRoute netId $
  NetRoute_TransferSearch :/ AccountParams
    { apToken = token
    , apAccount = account
    , apChain = Nothing
    }

getAccountDetail :: Text -> Maybe [AccountDetail]
getAccountDetail = A.decode . T.encodeUtf8 . fromStrict
