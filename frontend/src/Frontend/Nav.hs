{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav where

------------------------------------------------------------------------------
import           Control.Lens hiding (element)
import           Control.Monad.Fix
import           Data.Map (Map)
import           Data.Proxy
import           Data.Text (Text)
import           Obelisk.Generated.Static
import           Obelisk.Route.Frontend hiding (decode)
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types
------------------------------------------------------------------------------

nav
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Routed t r m)
  => NetId
  -> m ()
nav netId = do
  divClass "ui container" $ do
    elAttr "a" ("class" =: "header item" <>
                "href" =: "/" <>
                "style" =: "color: #e8098f;") $
      elAttr "img" ("class" =: "logo" <>
                    "src" =: static @"kadena-k-logo.png") $
        text "Kadena Block Explorer"
    elAttr "a" ("class" =: "header item" <> "href" =: "/") $ text "Kadena Block Explorer"
    divClass "right menu" $ do
      linkItem "About" "/about"
      getStarted
      learnMore
      networkWidget netId

getStarted
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => m ()
getStarted = mdo
  (e,_) <- elAttr' "div" ("class" =: "ui dropdown item") $ do
    text "Get Started"
    let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
    elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      linkItemNewTab "Start Mining" "https://github.com/kadena-io/chainweb-miner/blob/master/README.org"
      linkItemNewTab "Download Wallet" "https://www.kadena.io/chainweaver"
      linkItemNewTab "Play Testnet Games" "http://testnet.chainweb.com/games/"
      linkItemNewTab "See Chains in 3D (experimental)" (static @"chains-3d.html")
  dropdownVisible <- holdDyn False $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    ]
  return ()

learnMore
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => m ()
learnMore = mdo
  (e,_) <- elAttr' "div" ("class" =: "ui dropdown item") $ do
    text "Learn More"
    let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
    elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      linkItemNewTab "Kadena Docs" "https://docs.kadena.io"
      linkItemNewTab "Pact Smart Contract Tutorials" "https://pactlang.org"
      linkItemNewTab "Kadena Whitepapers" "https://docs.kadena.io/whitepapers/overview"
  dropdownVisible <- holdDyn False $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    ]
  return ()

linkItem
  :: DomBuilder t m
  => Text
  -> Text
  -> m ()
linkItem nm url = do
    elAttr "a" ("href" =: url <> "class" =: "item") $ text nm

linkItemNewTab
  :: DomBuilder t m
  => Text
  -> Text
  -> m ()
linkItemNewTab nm url = do
    elAttr "a" ("href" =: url <> "target" =: "_blank" <> "class" =: "item") $ text nm

networkName :: NetId -> Text
networkName NetId_Mainnet = "Mainnet"
networkName NetId_Testnet = "Testnet"
networkName (NetId_Custom _) = "Custom Network"

networkWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Routed t r m)
  => NetId
  -> m ()
networkWidget netId = mdo
  (e, _) <- elAttr' "div" ("class" =: "ui dropdown item") $ mdo
    text $ networkName netId
    let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
    elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      networkItem "Testnet" $ FR_Testnet :/ NetRoute_Chainweb :/ ()
      networkItem "Mainnet" $ FR_Mainnet :/ NetRoute_Chainweb :/ ()
  route <- askRoute
  dropdownVisible <- holdDyn False $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    , False <$ updated route
    ]
  pure ()


networkItem
  :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => Text -> R FrontendRoute -> m ()
networkItem t r = routeLinkAttr r ("class" =: "item") $ text t

routeLinkAttr
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     )
  => route -- ^ Target route
  -> Map AttributeName Text
  -> m a -- ^ Child widget
  -> m a
routeLinkAttr r attrs w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ attrs <> "href" =: enc r
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return a
