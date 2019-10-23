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
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics
import           Obelisk.Configs
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
  :: (DomBuilder t m, MonadHold t m, HasConfigs m, PostBuild t m,
      MonadFix m, Prerender js t m)
  => m (Dynamic t (Maybe Network))
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
      getStarted
      learnMore
      networkWidget
    --return $ constDyn DevNet

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
      linkItem "About Kadena Block Explorer" "/about"
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
  :: (DomBuilder t m, MonadHold t m, HasConfigs m, PostBuild t m,
      MonadFix m, Prerender js t m)
  => m (Dynamic t (Maybe Network))
networkWidget = mdo
  mnode <- getConfig "frontend/default-node"
  let host = decode . BL.fromStrict =<< mnode

  (e,net) <- elAttr' "div" ("class" =: "ui dropdown item") $ mdo
    dynText $ maybe "" humanize <$> curNet
    let mkAttrs as vis = "class" =: (if vis then (as <> " visible") else as)
    (dev,prod) <- elDynAttr "div" (mkAttrs "menu transition" <$> dropdownVisible) $ do
      d <- networkItem DevNet
      p <- networkItem ProdNet
      return (d,p)
    let netChange = Just <$> leftmost [prod, dev]
        chooseDefault cur = maybe (Just ProdNet) Just cur
    rec
        curNet <- fmap join $ prerender (return $ constDyn host) $ do
          pb <- getPostBuild
          mLastNet <- getItemStorage browserStorage localStorage NetworkState_LastUsed
          performEvent_ $ setItemStorage browserStorage localStorage NetworkState_LastUsed <$> fmapMaybe id netChange
          holdDyn mLastNet $ leftmost [netChange, chooseDefault <$> traceEvent "initial network override" (tag (current curNet) pb)]
    return curNet
  dropdownVisible <- holdDyn False $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    , False <$ updated net
    ]
  holdUniqDyn net


networkItem
  :: (DomBuilder t1 m, Humanizable a,
      HasDomEvent t2 (Element EventResult (DomBuilderSpace m) t1) 'ClickTag,
      Reflex t2)
  => a
  -> m (Event t2 a)
networkItem n = do
  (e,_) <- elAttr' "div" ("class" =: "item") $ text $ humanize n
  return (n <$ domEvent Click e)
