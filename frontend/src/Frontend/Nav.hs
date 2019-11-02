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
import           Control.Monad.Fix
import           Data.Text (Text)
import           Obelisk.Generated.Static
import           Obelisk.Route.Frontend hiding (decode)
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
------------------------------------------------------------------------------

nav
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Routed t r m)
  => m ()
nav = do
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
      networkWidget

getStarted
  :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => m ()
getStarted = mdo
  (e,_) <- elAttr' "div" ("class" =: "ui dropdown item") $ do
    text "Get Started"
    let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
    elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      linkItem "Start Mining" "https://github.com/kadena-io/chainweb-node/blob/master/miner/README.org"
      linkItem "Download Wallet" "http://testnet.chainweb.com/wallet/"
      linkItem "Play Testnet Games" "http://testnet.chainweb.com/games/"
      linkItem "See Chains in 3D (experimental)" (static @"chains-3d.html")
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
      linkItem "Pact Smart Contract Tutorials" "https://pactlang.org"
      linkItem "Kadena Whitepapers" "https://kadena.io/en/whitepapers/"
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

networkWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m,
      RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m, Routed t r m)
  => m ()
networkWidget = mdo
  (e, _) <- elAttr' "div" ("class" =: "ui dropdown item") $ mdo
    text "Network"
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
networkItem t r = routeLink r $ do
  elAttr "div" ("class" =: "item" <> "style" =: "color: black") $ text t
