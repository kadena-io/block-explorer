{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Data.Text.Read (decimal)
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
       Just _dataHost -> transferWidget aps (fromJust cw) -- We can assume the netconfig exists if we got to this branch!

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
  => AccountParams
  -> NetConfig
  -> App r t m ()
transferWidget AccountParams{..} nc = do
    pb <- getPostBuild
    res <- mkXhr (constDyn QNone) pb
    void $ networkHold (inlineLoader "Loading...") (f <$> res)
  where
    mkXhr nextToken_ evt =
      getTransfers
      nc
      (constDyn $ QParamSome $ Limit 20)
      (constDyn QNone)
      (constDyn $ Right apAccount)
      (constDyn $ QParamSome apToken)
      (constDyn $ maybe QNone (QParamSome . ChainId . fromIntegral) apChain)
      (constDyn $ maybe QNone (QParamSome . fromIntegral) apMinHeight)
      (constDyn $ maybe QNone (QParamSome . fromIntegral) apMaxHeight)
      nextToken_
        evt
    f = \case
        Left _ -> pure ()
        Right (accs, nextHeaderToken) -> mdo
          (AppState n _ _ _) <- ask
          elAttr "h2" ("data-tooltip" =: apAccount) $ text $ "Transfer Info"
          elClass "table" "ui definition table" $ do
            el "tbody" $ do
              tfield "Token" $ text apToken
              tfield "Account" $ accountSearchLink n apToken apAccount apAccount
              maybe (pure ()) (\cid -> tfield "Chain ID" $ text $ tshow cid) apChain
          let initialMinHeight = maybe "" tshow apMinHeight
              initialMaxHeight = maybe "" tshow apMaxHeight
          elClass "div" "ui labeled input" $ do
              elClass "div" "ui label" $ text "Minimum Height"
              minHeightInput <- inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
                      ("style" =: "border-radius: 0;" <> "placeholder" =: "Genesis")
                   & inputElementConfig_initialValue .~ initialMinHeight
              elClass "div" "ui label" $ text "Maximum Height"
              maxHeightInput <- inputElement $ def
                   & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
                      ("style" =: "border-radius: 0;" <> "placeholder" =: "Current Time")
                   & inputElementConfig_initialValue .~ initialMaxHeight
              let parseHeight :: Text -> Either String (Maybe Integer)
                  parseHeight (T.stripEnd -> a)
                   | T.null a = Right Nothing
                   | otherwise = case decimal @Integer a of
                       Left l -> Left l
                       Right (tt,rest)
                          | not (T.null rest) -> Left $ "parseHeight: unexpected suffix " <> T.unpack rest
                          | otherwise -> Right (Just tt)
              let buttonClass a b = case (parseHeight a, parseHeight b) of
                     (_, Left _) -> Left ("ui disabled button", "max height is not an integer")
                     (Left _, _) -> Left ("ui disabled button", "min height is not an integer")
                     (Right (Just parsedA), Right (Just parsedB))
                         | not (parsedA <= parsedB) ->
                           Left ("ui disabled button", "Min height must be less than or equal to Max height")
                     (Right parsedA, Right parsedB)
                        | a == initialMinHeight && b == initialMaxHeight ->
                            Left ("ui disabled button", "The height range has not changed!")
                        | otherwise -> Right ("ui button", (parsedA,parsedB))
              let onEnter w = if w == 13 then Just () else Nothing
              let filterButtonWidget a b = case buttonClass a b of
                   Left (tt,errToolTip) -> void
                       $ elAttr "span" ("data-tooltip" =: errToolTip)
                       $ elAttr' "button" ("class" =: tt) $ text "Filter results"
                   Right (enabled, (minHeight,maxHeight)) -> do
                       (d,_) <- elAttr' "button" ("class" =: enabled) $ text "Filter results"
                       let route = mkTransferSearchRoute n apAccount apToken apChain minHeight maxHeight
                       setRoute
                           $ route
                           <$ leftmost
                           [ mapMaybe onEnter $ domEvent Keypress minHeightInput
                           , mapMaybe onEnter $ domEvent Keypress maxHeightInput
                           , domEvent Click d]
              dyn_ $ zipDynWith filterButtonWidget
                  (_inputElement_value minHeightInput)
                  (_inputElement_value maxHeightInput)
          t <- elClass "table" "ui compact celled table" $ do
            el "thead" $ el "tr" $ do
              maybe (el "th" $ text "Chain") (const $ pure ()) apChain
              elAttr "th" ("style" =: "width: auto") $ text "Time"
              elAttr "th" ("style" =: "width: auto") $ text "Height"
              elAttr "th" ("style" =: "width: auto") $ text "Request Key"
              elAttr "th" ("style" =: "width: auto") $ text "From/To"
              elAttr "th" ("style" =: "width: auto; text-align: right;") $ text "Amount"
            el "tbody" $ mdo
              let rowsToRender tds = forM_ tds $ \td ->
                    el "tr" $ drawRow n apToken apAccount apChain decimalPointsDyn td
              rowsToRender accs
              let maxDecimalPoints tds = maximum $ 0 : fmap (decimalPoints . _trDetail_amount) tds
              p <- produceNewRowsOnToken mkXhr e
              let (errorE, goodE) = fanEither p
              let (details,newToken) = splitE goodE
                  gatherDecimalPoints oldDecPoints newDetails = max oldDecPoints (maxDecimalPoints newDetails)
              decimalPointsDyn :: Dynamic t Int <- accum gatherDecimalPoints (maxDecimalPoints accs) details
              indexWithRender <- accum (\(key,_oldDetails) newDetails -> (succ key, rowsToRender newDetails)) (0 :: Integer, pure ()) details
              void $ listHoldWithKey mempty (indexWithRender <&> \(i,r) -> M.singleton i (Just r)) (\_ r -> r)
              return $ leftmost [fmap Left errorE, fmap Right newToken]
          e <- case nextHeaderToken of
            Nothing -> pure never
            Just (NextToken next) -> elAttr "div" ("style" =: "display: grid;") $ evaporateButtonOnClick next t
          pure ()

displayAmount :: Scientific-> Text
displayAmount s = T.pack $ formatScientific Fixed Nothing s

decimalPoints :: StringEncoded Scientific-> Int
decimalPoints (StringEncoded s) = case T.splitOn "." $ displayAmount s of
  [_,b] -> T.length b
  _ -> 0

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
           Dynamic t (QParam NextToken)
        -> Event t ()
        -> m (Event t (Either TransferError ([TransferDetail], Maybe NextToken)))

produceNewRowsOnToken
  :: (MonadApp r t m, MonadJSM (Performable m),
  HasJSContext (Performable m), Prerender js t m,
  RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, MonadIO m)
  => TransferRequester t m -> Event t Text -> m (Event t (Either TransferError ([TransferDetail], Maybe Text)))
produceNewRowsOnToken mkXhr nextToken_ = do
    newTokenDyn <- holdDyn QNone (QParamSome . NextToken <$> nextToken_)
    mkXhr newTokenDyn (() <$ nextToken_) <&&> \case
            Left err -> Left err
            Right (details, nextTokenHeader) -> Right (details, unNextToken <$> nextTokenHeader)
  where
    (<&&>) = flip (fmap . fmap)

data TokenMovement
  = Unknown Text
  | Coinbase
  | Incoming (Either Text OtherAccount)
  | Outgoing (Either Text OtherAccount)

data OtherAccount = OtherAccount
  { _oa_chainId :: Maybe Int
  , _oa_account :: Text
  }

decideTokenMovement :: Text -> TransferDetail -> TokenMovement
decideTokenMovement account tr
  | _trDetail_requestKey tr == "<coinbase>" = Coinbase
  | _trDetail_fromAccount tr == account = Outgoing $ determineOther (_trDetail_toAccount tr)
  | _trDetail_toAccount tr == account = Incoming $ determineOther (_trDetail_fromAccount tr)
  | otherwise = Unknown "Could not determine token movement"
  where
    determineOther other =
      case (other, _trDetail_crossChainId tr, _trDetail_crossChainAccount tr) of
      (_, Just _, Nothing) ->
        Left "Cross chain account is missing but cross chain id is present"
      (_, Nothing, Just _) ->
        Left "Cross chain id is missing but cross chain account is present"
      ("", Just chain, Just otherAccount) -> Right $ OtherAccount (Just chain) otherAccount
      ("", Nothing, Nothing) ->
        Left "Could not determine the other account"
      (_, _, _) -> Right $ OtherAccount Nothing other

tmAmountClass :: TokenMovement -> Text
tmAmountClass tm = "token-amount " <> case tm of
  Coinbase -> "token-amount-incoming"
  Incoming _ -> "token-amount-incoming"
  Outgoing _ -> "token-amount-outgoing"
  Unknown _ -> "token-amount-unknown"

tmAmountNegate :: TokenMovement -> Bool
tmAmountNegate = \case
  Coinbase -> False
  Incoming _ -> False
  Outgoing _ -> True
  Unknown _ -> False

tmTooltip :: TokenMovement -> Text
tmTooltip = \case
  Coinbase -> "Rewarded for mining"
  Incoming eiOther -> case eiOther of
    Left reason -> "Failed to determine the sender, please inspect the transaction:\n" <> reason
    Right other -> case _oa_chainId other of
      Just chainId -> "Cross chain transaction received from " <> _oa_account other <> " on chain " <> tshow chainId
      Nothing -> "Transaction received from " <> _oa_account other
  Outgoing eiOther -> case eiOther of
    Left reason -> "Failed to determine the recipient, please inspect the transaction:\n" <> reason
    Right other -> case _oa_chainId other of
      Just chainId -> "Cross chain transaction sent to " <> _oa_account other <> " on chain " <> tshow chainId
      Nothing -> "Transaction sent to " <> _oa_account other
  Unknown reason -> "Failed to determine, please inspect the transaction: " <> reason

drawRow
  :: MonadFix m
   => MonadHold t m
   => RouteToUrl (R FrontendRoute) m
   => SetRoute t (R FrontendRoute) m
   => DomBuilder t m
   => Prerender js t m
   => PostBuild t m
   => NetId -> Text -> Text -> Maybe Integer -> Dynamic t Int -> TransferDetail -> m ()
drawRow n token account chainid decimalPointsDyn acc = mdo
  let hash = _trDetail_blockHash acc
      requestKey = _trDetail_requestKey acc
      cid = _trDetail_chain acc
      height = _trDetail_height acc
      StringEncoded amount = _trDetail_amount acc
      timestamp = _trDetail_blockTime acc
      tokenMovement = decideTokenMovement account acc
  when (isNothing chainid) $ elAttr "td" ("data-label" =: "Chain" <> "style" =: "white-space: nowrap;width: 0px;") $ text $ tshow cid
  elAttr "td" ("data-label" =: "Time" <> "style" =: "white-space: nowrap; width: 0px;") $ text $ T.pack $ formatTime defaultTimeLocale "%F %T" timestamp
  elAttr "td" ("data-label" =: "Block Height" <> "data-tooltip" =: hash <> "style" =: "white-space: nowrap; width: 0px;") $
    blockHashLink n (ChainId $ fromIntegral cid) hash (tshow height)
  let cutText = elAttr "div" ("class" =: "cut-text")
  elAttr "td"
    ( "data-label" =: "Request Key"
   <> "style" =: "max-width: 150px; padding: 0px"
   <> "data-tooltip" =: requestKey
    ) $ cutText $ if requestKey == "<coinbase>"
        then text "Coinbase"
        else txDetailLink n requestKey requestKey
  -- The From/To column is a bit more complicated because we want all of the following to work:
  -- 1. If the account name is too long to fit in the column, we want to cut it off with ellipsis
  -- 2. We want to have a tooltip for the whole cell
  -- 3. We want to have specialized tooltips when the user hovers over the tags
  -- The problem is that in order for the text cut off to work, the content needs to be surrounded
  -- by a div with overflow: hidden. However, this also hides the tooltips of the child elements.
  -- so we want the tooltip to be set outside of the div. That's why we have the tooltipOverride
  -- dynamic, which is used by the tag hover handlers to override the tooltip.
  let fromToCell override = elDynAttr "td" $ override <&> \mbMsg ->
           M.singleton "data-label" "From/To"
        <> M.singleton "style" "max-width: 250px; padding: 0px;"
        <> M.singleton "data-tooltip" (fromMaybe (tmTooltip tokenMovement) mbMsg)
  let puzzleEmoji = "\x1F9E9"
      chainEmoji = "\x1F517"
  tooltipOverride <- fromToCell tooltipOverride $ do
    let mkTag txt tooltip = do
          (e,_) <- elAttr' "span" ("class" =:"cross-chain-tag") $ text txt
          isHoveringDyn <- hoverDyn e
          return $ isHoveringDyn <&> \isHovering -> if isHovering then Just tooltip else Nothing
        fromMaybeDyn = fromMaybe (constDyn Nothing)
    cutText $ case tokenMovement of
      Coinbase -> mkTag "Coinbase" "Rewarded for mining"
      Incoming eiOther -> do
        text "From: "
        case eiOther of
          Left reason -> do
            mkTag "Unknown" $ "Failed to determine the sender, please inspect the transaction:\n" <> reason
          Right other -> do
            hoveringChainLabel <- fmap fromMaybeDyn $ forM (_oa_chainId other) $ \chainId ->
                mkTag (chainEmoji <> tshow chainId) "This account is on a different chain"
            accountSearchLink n token (_oa_account other) (_oa_account other)
            return hoveringChainLabel
      Outgoing eiOther -> do
        text "To: "
        case eiOther of
          Left reason -> do
            mkTag "Unknown" $ "Failed to determine the recipient, please inspect the transaction:\n" <> reason
          Right other -> do
            hoveringChainLabel <- fmap fromMaybeDyn $ forM (_oa_chainId other) $ \chainId ->
                mkTag (puzzleEmoji <> chainEmoji <> tshow chainId) "Completion is required for this transaction involving an account on a different chain."
            accountSearchLink n token (_oa_account other) (_oa_account other)
            return hoveringChainLabel
      Unknown _ -> do
        mkTag "Unknown" "Failed to interpret the transfer, please inspect the transaction"
  elAttr "td" ("data-label" =: "Amount" <> "class" =: tmAmountClass tokenMovement <> "style" =: "text-align: right;") $ do
    let nonBreakingSpace = "\x00A0"
        signedAmount = if tmAmountNegate tokenMovement then (negate amount) else amount
        mkPadding = dynText $ decimalPointsDyn <&> \tableDecimalPoints ->
          let thisDecimalPoints = decimalPoints $ _trDetail_amount acc
              missingPoints = tableDecimalPoints - thisDecimalPoints
          in T.replicate missingPoints nonBreakingSpace
    case T.splitOn "." $ displayAmount signedAmount of
      [whole,decimals] -> do
        text $ whole
        elClass "em" "token-amount-decimal" $ do
          text $ "." <> decimals
          mkPadding
      [whole] -> do
        text whole
        elClass "em" "token-amount-decimal" $ do
          text nonBreakingSpace
          mkPadding
      _ -> text $ displayAmount signedAmount -- Impossible case because displayAmount always returns a decimal number

mkTransferSearchRoute :: NetId -> Text -> Text -> Maybe Integer -> Maybe Integer -> Maybe Integer -> R FrontendRoute
mkTransferSearchRoute netId account token chain minheight maxheight = mkNetRoute netId $
  NetRoute_TransferSearch :/ AccountParams
    { apToken = token
    , apAccount = account
    , apChain = chain
    , apMinHeight = minheight
    , apMaxHeight = maxheight
    }

getTransferDetail :: Text -> Maybe [TransferDetail]
getTransferDetail = A.decode . T.encodeUtf8 . fromStrict
