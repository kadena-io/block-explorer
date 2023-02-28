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
import           Data.Time.Format
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
import           Chainweb.Api.StringEncoded
import           ChainwebData.TransferDetail
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
transferHelper aps = do
     (AppState _ _ cw _) <- ask
     case cw >>= _netConfig_dataHost of
       Nothing -> text "Transfers view is not supported unless a chainweb-data url is included in the config!"
       Just _dataHost -> transferWidget (apAccount aps) (apToken aps) (apChain aps) (fromJust cw) -- We can assume the netconfig exists if we got to this branch!

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
  -> App r t m ()
transferWidget account token chainid nc = do
    pb <- getPostBuild
    res <- mkXhr QNone QNone (constDyn QNone) pb
    void $ networkHold (inlineLoader "Loading...") (f <$> res)
  where
    mkXhr lim off nextToken_ evt =
      getTransfers
      nc
      (constDyn lim)
      (constDyn off)
      (constDyn $ Right account)
      (constDyn $ QParamSome token)
      (constDyn $ maybe QNone (QParamSome . ChainId . fromIntegral) chainid)
      nextToken_
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
            t <- elClass "table" "ui compact celled table" $ do
              el "thead" $ el "tr" $ do
                maybe (el "th" $ text "Chain") (const $ pure ()) chainid
                elAttr "th" ("style" =: "width: auto") $ text "Time"
                elAttr "th" ("style" =: "width: auto") $ text "Height"
                elAttr "th" ("style" =: "width: auto") $ text "Request Key"
                elAttr "th" ("style" =: "width: auto") $ text "From/To"
                elAttr "th" ("style" =: "width: auto") $ text "Amount"
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
        Left (BadResponse txt status) -> disabledButton ("Bad response (status: " <> tshow status <> "): " <> txt) >> pure never
        Left (ReqFailure txt) -> disabledButton ("Request Failure: " <> txt) >> pure never
  beenClicked <- holdDyn ((const token) <$$> fetchButton nextButtonText) something
  clickNow <- networkView beenClicked
  click <- switchHold never clickNow
  pure click

type TransferRequester t m =
        QParam Limit
        -> QParam Offset
        -> Dynamic t (QParam NextToken)
        -> Event t ()
        -> m (Event t (Either TransferError ([TransferDetail], Maybe NextToken)))

produceNewRowsOnToken
  :: (MonadApp r t m, MonadJSM (Performable m),
  HasJSContext (Performable m), Prerender js t m,
  RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, MonadIO m)
  => TransferRequester t m -> Event t Text -> m (Event t (Either TransferError ([TransferDetail], Maybe Text)))
produceNewRowsOnToken mkXhr nextToken_ = do
    newTokenDyn <- holdDyn QNone (QParamSome . NextToken <$> nextToken_)
    mkXhr QNone QNone newTokenDyn (() <$ nextToken_) <&&> \case
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
   => NetId -> Text -> Text -> Maybe Integer -> TransferDetail -> m ()
drawRow n token account chainid acc = do
  let hash = _trDetail_blockHash acc
      requestKey = _trDetail_requestKey acc
      cid = _trDetail_chain acc
      height = _trDetail_height acc
      fromAccount = _trDetail_fromAccount acc
      toAccount = _trDetail_toAccount acc
      StringEncoded amount = _trDetail_amount acc
      timestamp = _trDetail_blockTime acc
  when (isNothing chainid) $ elAttr "td" ("data-label" =: "Chain" <> "style" =: "white-space: nowrap;width: 0px;") $ text $ tshow cid
  elAttr "td" ("data-label" =: "Time" <> "style" =: "white-space: nowrap; width: 0px;") $ text $ T.pack $ formatTime defaultTimeLocale "%F %T" timestamp
  elAttr "td" ("data-label" =: "Block Height" <> "data-tooltip" =: hash <> "style" =: "white-space: nowrap; width: 0px;") $
    blockHashLink n (ChainId $ fromIntegral cid) hash (tshow height)
  elAttr "td" ("class" =: "cut-text" <> "data-label" =: "Request Key" <> "data-tooltip" =: requestKey) $
    if requestKey == "<coinbase>" then text "coinbase" else txDetailLink n requestKey requestKey
  let showAccount = listToMaybe [a | a <- [fromAccount, toAccount], a /= account, not $ T.null a]
  elAttr "td" ("class" =: "cut-text" <> "data-label" =: "From/To" <> foldMap (\s -> "data-tooltip" =: s) showAccount) $
    case showAccount of
      Nothing -> pure ()
      Just s -> do
        text $ if s == fromAccount then "From: " else "To: "
        accountSearchLink n token s s
  let isNegAmt = fromAccount == account
  elAttr "td" ("data-label" =: "Amount" <> "style" =: ((if isNegAmt then "color: red;" else "color: green;") <> "white-space: nowrap;width: 0px;")) $ do
    let printedAmount amt = formatScientific Fixed Nothing amt
    text $ T.pack $ if isNegAmt then printedAmount (negate amount) else printedAmount amount

mkTransferSearchRoute :: NetId -> Text -> Text -> R FrontendRoute
mkTransferSearchRoute netId account token = mkNetRoute netId $
  NetRoute_TransferSearch :/ AccountParams
    { apToken = token
    , apAccount = account
    , apChain = Nothing
    }

getTransferDetail :: Text -> Maybe [TransferDetail]
getTransferDetail = A.decode . T.encodeUtf8 . fromStrict
