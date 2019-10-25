{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.About where

------------------------------------------------------------------------------
import           Reflex.Dom
import           Obelisk.Generated.Static
------------------------------------------------------------------------------

aboutWidget :: (Monad m, DomBuilder t m) => m ()
aboutWidget = divClass "ui text container" $ do
  el "p" $ text "Block Explorer is an analytics tool for the Kadena platform which visualizes the mining, propagation and braiding of blocks across multiple Kadena chains in real time."
  elAttr "img" ("src" =: static @"AboutBlockExplorer.png" <> "style" =: "width: 100%;") blank
  elClass "ol" "ui list" $ do
    el "li" $ text "The name of the network that is currently displaying block explorer data"
    el "li" $ text "Estimate of total network hash rate"
    el "li" $ text "The sum of transactions within mined blocks since the web page was last loaded"
    el "li" $ text "Transaction per second (TPS): Here is it calculated by transactions received divided by the number of seconds since the web page was last loaded"
    el "li" $ text "Block Height indicates the length of the blockchain"
    el "li" $ text "The expected number of hashes to mine a block on this chain"
    el "li" $ text "Block ID is the hash of the block header"
    el "li" $ text "Time that the miner started mining the block"
    el "li" $ text "The number of transactions mined within the block of a given chain"
  el "p" $ text "The Kadena Public Blockchain runs a protocol called Chainweb, a novel parallel-chain Proof-of-Work architecture comprised of braided chains that all mine the same native currency and transfer liquidity between each other."
  el "p" $ text "In Chainweb, the braided chains incorporate Merkle proofs from adjacent chains in a fixed graph layout that ensures that proofs quickly propagate to every other chain in the system within some maximum block depth."
  el "p" $ text "Hover your cursor over any block to highlight the neighbor chains that are cryptographically linked to it. In this 10 chain configuration, notice that all 10 chains become braided together within the height of two blocks."
  el "p" $ text "By linking multiple chains together in this manner, Kadena offers massive throughput and enhanced security."
