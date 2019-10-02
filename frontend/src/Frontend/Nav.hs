{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

------------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
--import           Data.Universe (universe)
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.Common
------------------------------------------------------------------------------

leftMenuItems :: [Some.Some FrontendRoute]
leftMenuItems = []

nav
  :: (MonadApp r t m)
  => m ()
nav = do
  divClass "ui container" $ do
    elAttr "a" ("class" =: "header item" <>
                "href" =: "/" <>
                "style" =: "color: #e8098f;") $
      --text "Chainscan"
      elAttr "img" ("class" =: "logo" <>
                    "src" =: static @"kadena-k-logo.png") $
        text "Chainscan"
    elAttr "a" ("class" =: "header item" <> "href" =: "/") $ text "Chainscan"
    divClass "right menu" $ do
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Miners"
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Developers"
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Resources"
    return ()


-- Create a link that is highlighted if it is the current tab
--menuItem
--  :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m,
--      SetRoute t (R FrontendRoute) m, PostBuild t m)
--  => Dynamic t (DSum FrontendRoute f)
--  -> Some.Some FrontendRoute
--  -> m ()
--menuItem currentTab tab = do
--    let currentTabDemux = demux $ fmap (\(t :=> _) -> Some.Some t) currentTab
--    let thisTabIsSelected = demuxed currentTabDemux tab
--        highlightAttrs = ffor thisTabIsSelected $ \case
--          True -> "class" =: "active clickable item"
--          False -> "class" =: "clickable item"
--    internalLink (tabHomepage tab) $ \cfg ->
--      elDynAttr' "span" highlightAttrs $ element "a" cfg (tabTitle tab)
--    return ()
