{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Frontend.Accounts where

------------------------------------------------------------------------------
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Foldable
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Data.Decimal
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Lens (unpacked)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
import           Servant.Reflex
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           ChainwebData.Api
import           ChainwebData.EventDetail
import           ChainwebData.Pagination
import           Common.Route
import           Common.Types
import           Common.Utils
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
import           Frontend.Page.Block
import           Frontend.Transactions
import           PactNumber
------------------------------------------------------------------------------

accountSearchPage
  :: ( MonadApp r t m
     , Prerender js t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadIO m
     )
  => App AccountParams t m ()
accountSearchPage = do
    r <- askRoute
    void $ networkView $ accountHelper <$> r

accountHelper
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
accountHelper aps = case apChain aps of
  Nothing -> accountWidget token account
  Just chain -> accountChainWidget token account chain
  where token = apToken aps
        account = apAccount aps

accountWidget
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
  -> App r t m ()
accountWidget token account = do
  (AppState n si _ _) <- ask
  let chains = S.toList $ _siChains si
      chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
  curTime <- liftIO getCurrentTime
  let meta chain = ChainwebMeta
        (tshow $ unChainId chain)
        (utcTimeToPOSIXSeconds $ addUTCTime (- 60) curTime)
        600 -- 10 minutes
        1200 -- Decent margin for error
        0.001 -- High since this is a local and it shouldn't matter
        "dummy-sender"
      mkXhr chain = detailsXhr chainwebHost (meta chain) token account
  case sequence (map mkXhr chains) of
    Left e -> el "div" $ text $ "Error constructing XHR: " <> T.pack e
    Right xhrs -> do
      pb <- getPostBuild
      results <- performRequestsAsync $ xhrs <$ pb
      rds <- fmap (getDetails . _xhrResponse_responseText) <$$$> holdDyn Nothing (Just <$> results)
      el "h2" $ text "Account Info"

      elClass "table" "ui definition table" $ do
        el "tbody" $ do
          el "tr" $ do
            elClass "td" "two wide" $ text "Token"
            el "td" $ text token
          el "tr" $ do
            elClass "td" "two wide" $ text "Account"
            el "td" $ text account
      _ <- networkView (accountInfo token account <$> rds)
      pure ()

data ChainInfo = ChainInfo
  { _chainInfo_totalBalance :: Decimal
  , _chainInfo_chainBalances :: Map Integer Decimal
  }

addValue :: a -> ChainInfo -> ChainInfo -> ChainInfo
addValue _chain new old = ChainInfo
  (_chainInfo_totalBalance old + _chainInfo_totalBalance new)
  (M.union (_chainInfo_chainBalances old) (_chainInfo_chainBalances new))


addTo :: (Eq k, Hashable k) => HM.HashMap k ChainInfo -> (Integer, k, PactNumber) -> HM.HashMap k ChainInfo
addTo m (c,g,bal) = HM.insertWith (addValue c) g (ChainInfo d (M.singleton c d)) m
  where
    d = pactNumberToDecimal bal

accountInfo
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
  -> Maybe [Maybe (Integer, A.Value, PactNumber)]
  -> App r t m ()
accountInfo token account mInfos = do
    (AppState n _si _mdbh _) <- ask
    case mInfos of
      Nothing -> inlineLoader "Loading..."
      Just infos -> do
        let good = catMaybes infos
            goodCount = length good
            totalCount = length infos
            --balances = M.unionsWith (+) $ map (\(k,amt) -> M.singleton (encode k) (pactNumberToDecimal amt)) good
            --addValue chain (newCoins, newCount) (oldCoins, oldCount) = (oldCoins + newCoins, oldCount + newCount)
            balances = foldl' addTo mempty good
        let linkText = "View most recent transfers associated to this account."
        el "p" $ routeLink (mkTransferViewRoute n account token Nothing) (text linkText)
        el "p" $ do
          text $ "Got data from " <> tshow goodCount <> " chains"
          when (totalCount > goodCount) $
            text $ "(failed to get results from " <> tshow (totalCount - goodCount) <> " chains)"
        elClass "table" "ui celled table" $ do
          el "thead" $ do
            el "tr" $ do
              el "th" blank
              el "th" $ text "Keyset"
              el "th" $ text "Balance"
          el "tbody" $ do
            forM_ (HM.toList balances) $ \(g,cinfo) -> do
              openDyn <- el "tr" $ mdo
                let mkAttrs open = if open then "class" =: "angle down icon" else "class" =: "angle right icon"
                (e,_) <- el' "td" $ elDynAttr "i" (mkAttrs <$> openDyn) blank
                openDyn <- toggle False (domEvent Click e)
                el "td" $ jsonTable g
                elClass "td" "three wide" $ do
                  text $ tshow $ _chainInfo_totalBalance cinfo
                  el "br" blank
                  text $ "spread across " <> tshow (M.size $ _chainInfo_chainBalances cinfo) <> " chains"
                return openDyn
              forM_ (M.toList $ _chainInfo_chainBalances cinfo) $ \(chain, bal) -> do
                let mkAttrs open = if open then "class" =: "selectable-row" <> "style" =: "cursor: pointer;" else "class" =: "selectable-row" <> "style" =: "display: none;"
                (e,_) <- elDynAttr' "tr" (mkAttrs <$> openDyn) $ do
                  el "td" blank
                  el "td" $ routeLink (mkTransferViewRoute n account "coin" (Just chain)) $
                    text $ "Chain " <> T.pack (show chain)
                  el "td" $ text $ tshow bal
                let setChainRoute evt = setRoute $
                      mkAccountRoute n token account (Just chain) <$ evt
                setChainRoute (domEvent Click e)

getDetails :: Maybe Text -> Maybe (Integer, A.Value, PactNumber)
getDetails mt = do
    t <- mt
    chain <- t ^? _Value . key "metaData" . key "publicMeta" . key "chainId" . _String . unpacked . _Show
    d <- t ^? _Value . key "result" . key "data"
    g <- d ^? key "guard"
    bal <- d ^? key "balance"
    dec <- A.parseMaybe A.parseJSON bal
    pure (chain, g, dec)

qParam :: Text
qParam = "q"

limParam :: Text
limParam = "lim"

pageParam :: Text
pageParam = "page"

accountChainWidget
  :: ( MonadApp r t m
     , DomBuilder t m
     , MonadJSM (Performable m)
     , HasJSContext (Performable m)
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     , MonadIO m
     )
  => Text
  -> Text
  -> Integer
  -> App r t m ()
accountChainWidget token account _chain = do
  (AppState n si mdbh _) <- ask
  let _chains = S.toList $ _siChains si
      _chainwebHost = ChainwebHost (netHost n) (_siChainwebVer si)
  case mdbh of
    Nothing -> text "Event search feature not available for this network"
    Just dbh -> do
      pb <- getPostBuild
      --pmap <- askRoute
      --let page = do
      --      pm <- pmap
      --      pure $ fromMaybe 1 $ readMaybe . T.unpack =<< join (M.lookup pageParam pm)
      res <- searchEvents dbh
          (constDyn $ Just $ Limit itemsPerPage)
          (Just . Offset . (*itemsPerPage) . pred <$> constDyn 1)
          (constDyn QNone)
          (QParamSome <$> constDyn (EventParam $ token <> ".TRANSFER"))
          (QParamSome <$> constDyn (EventName account))
          (constDyn QNone)
          (constDyn QNone)
          pb
      --divClass "ui pagination menu" $ do
      --  let setSearchRoute f e = setRoute $
      --        tag (current $ mkEventSearchRoute n <$> needle <*> fmap (Just . f) page) e
      --      prevAttrs p = if p == 1
      --                      then "class" =: "disabled item"
      --                      else "class" =: "item"
      --  (p,_) <- elDynAttr' "div" (prevAttrs <$> page) $ text "Prev"
      --  setSearchRoute pred (domEvent Click p)
      --  divClass "disabled item" $ display page
      --  (next,_) <- elAttr' "div" ("class" =: "item") $ text "Next"
      --  setSearchRoute succ (domEvent Click next)

      let f = either text (accountHistTable n)
      void $ networkHold (inlineLoader "Querying blockchain...") (f <$> res)

--accountSearch
--    :: ( MonadApp r t m
--       , Prerender js t m
--       , MonadJSM (Performable m)
--       , HasJSContext (Performable m)
--       , RouteToUrl (R FrontendRoute) m
--       , SetRoute t (R FrontendRoute) m
--       )
--    => m ()
--accountSearch = do
--    pmap <- askRoute
--    pb <- getPostBuild
--    let page = do
--          pm <- pmap
--          pure $ fromMaybe 1 $ readMaybe . T.unpack =<< join (M.lookup pageParam pm)
--        needle = do
--          pm <- pmap
--          pure $ fromMaybe "" $ join (M.lookup qParam pm)
--        newSearch = leftmost [pb, () <$ updated pmap]
--    res <- searchEvents dbh
--        (constDyn $ QParamSome $ Limit itemsPerPage)
--        (QParamSome . Offset . (*itemsPerPage) . pred <$> page)
--        (QParamSome <$> needle)
--        (constDyn QNone)
--        (constDyn QNone)
--        newSearch
--    divClass "ui pagination menu" $ do
--      let setSearchRoute f e = setRoute $
--            tag (current $ mkEventSearchRoute n <$> needle <*> fmap (Just . f) page) e
--          prevAttrs p = if p == 1
--                          then "class" =: "disabled item"
--                          else "class" =: "item"
--      (p,_) <- elDynAttr' "div" (prevAttrs <$> page) $ text "Prev"
--      setSearchRoute pred (domEvent Click p)
--      divClass "disabled item" $ display page
--      (next,_) <- elAttr' "div" ("class" =: "item") $ text "Next"
--      setSearchRoute succ (domEvent Click next)
--
--    let f = either text (evTable n)
--    void $ networkHold (inlineLoader "Querying blockchain...") (f <$> res)

accountHistTable
  :: ( DomBuilder t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
     )
  => NetId
  -> (Bool, [EventDetail])
  -> m ()
accountHistTable _ (t, []) = do
  el "h4" $ text "Event Search"
  if t then text "No results." else inlineLoader "Loading new results ..."
accountHistTable net (t, evs) = do
  el "h4" $ text "Event Search"
  elClass "table" "ui compact celled table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ text "Block Time"
      el "th" $ text "Height"
      el "th" $ text "Tx"
      el "th" $ text "Event"
      el "th" $ text "Parameters"
    el "tbody" $ do
      forM_ evs $ \ev -> el "tr" $ do
        let chain = _evDetail_chain ev
        let height = _evDetail_height ev
        let rk = T.take 10 (_evDetail_requestKey ev) <> "..."
        elAttr "td" ("data-label" =: "Chain") $
            text $ tshow $ _evDetail_blockTime ev
        elAttr "td" ("data-label" =: "Height") $
            blockLink net (ChainId chain) height $ tshow height
        elAttr "td" ("data-label" =: "Tx") $
            txDetailLink net (_evDetail_requestKey ev) rk
        elAttr "td" ("data-label" =: "Event") $
            text $ _evDetail_name ev
        elAttr "td" ("data-label" =: "Parameters") $ el "pre" $
            text $ T.intercalate "\n" (map pactValueJSON $ _evDetail_params ev)
  unless t $ inlineLoader "Loading new rows..."

mkTransferViewRoute :: NetId -> Text -> Text -> Maybe Integer -> R FrontendRoute
mkTransferViewRoute netId account token chainid =
  mkNetRoute netId $ NetRoute_TransferSearch :/ AccountParams
    { apToken = token
    , apAccount = account
    , apChain = chainid
    }

mkAccountRoute :: NetId -> Text -> Text -> Maybe Integer -> R FrontendRoute
mkAccountRoute netId token account chain = mkNetRoute netId $
  NetRoute_AccountSearch :/ AccountParams
    { apToken = token
    , apAccount = account
    , apChain = chain
    }

accountSearchLink
  :: (RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m,
      DomBuilder t m,
      Prerender js t m
     )
  => NetId
  -> Text
  -> Text
  -> Text
  -> m ()
accountSearchLink netId token account linkText =
  routeLink (mkAccountRoute netId token account Nothing) $ text linkText
