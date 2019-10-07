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
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           Obelisk.Generated.Static
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Utils
import           Frontend.ChainwebApi
import           Frontend.Storage
------------------------------------------------------------------------------

data Network = DevNet | ProdNet | CustomNet ChainwebHost
  deriving (Eq,Ord,Show,Generic)

instance ToJSON Network where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Network

devHost :: ChainwebHost
devHost = ChainwebHost (Host "us3.tn1.chainweb.com" 443) Development

prodHost :: ChainwebHost
prodHost = ChainwebHost (Host "eu2.testnet.chainweb.com" 443) Testnet02

networkHost :: Network -> ChainwebHost
networkHost DevNet = devHost
networkHost ProdNet = prodHost
networkHost (CustomNet ch) = ch

instance Humanizable Network where
  humanize DevNet = "Kadena Dev Testnet"
  humanize ProdNet = "Kadena Prod Testnet"
  humanize (CustomNet ch) = hostAddress $ chHost ch

-- | Storage keys for referencing data to be stored/retrieved.
data NetworkState a where
  NetworkState_LastUsed :: NetworkState Network

deriving instance Show (NetworkState a)

nav
  :: ( DomBuilder t m, PostBuild t m, Prerender js t m
     , MonadHold t m , MonadFix m
     )
  => m (Dynamic t Network)
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
    divClass "right menu" $ mdo
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Miners"
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Developers"
      elAttr "a" ("class" =: "item" <> "href" =: "#") $ text "Resources"
      -- (e,net) <- elAttr' "div" ("class" =: "ui dropdown item") $ mdo
      --   dynText $ humanize <$> curNet
      --   elClass "i" "dropdown icon" blank
      --   let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
      --   (dev,prod) <- elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      --     d <- networkItem DevNet
      --     p <- networkItem ProdNet
      --     return (d,p)
      --   let netChange = leftmost [prod, dev]
      --   curNet <- fmap join $ prerender (return $ constDyn ProdNet) $ do
      --     mLastNet <- getItemStorage browserStorage localStorage NetworkState_LastUsed
      --     pb <- getPostBuild
      --     performEvent_ (liftIO (print mLastNet) <$ pb)
      --     performEvent_ $ setItemStorage browserStorage localStorage NetworkState_LastUsed <$> netChange
      --     holdDyn (fromMaybe ProdNet mLastNet) netChange
      --   return curNet
      -- dropdownVisible <- holdDyn False $ leftmost
      --   [ True <$ domEvent Mouseenter e
      --   , False <$ domEvent Mouseleave e
      --   , False <$ updated net
      --   ]
      -- return net
      holdDyn DevNet never

networkItem
  :: (DomBuilder t1 m, Humanizable a,
      HasDomEvent t2 (Element EventResult (DomBuilderSpace m) t1) 'ClickTag,
      Reflex t2)
  => a
  -> m (Event t2 a)
networkItem n = do
  (e,_) <- elAttr' "div" ("class" =: "item") $ text $ humanize n
  return (n <$ domEvent Click e)
