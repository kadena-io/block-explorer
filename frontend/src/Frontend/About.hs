{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.About where

------------------------------------------------------------------------------
import           Reflex.Dom
import           Obelisk.Generated.Static
------------------------------------------------------------------------------

aboutWidget :: DomBuilder t m => m ()
aboutWidget = divClass "ui text container" $ do
  el "h2" $ text "What is the Kadena Block Explorer?"
  el "p" $ text "Block Explorer is an analytics tool for the Kadena platform which visualizes the mining, propagation and braiding of blocks across multiple Kadena chains in real time and allows you to search for transactions and explore the contents of blocks."

  el "h2" $ text "Understanding the interface"
  elAttr "img" ("src" =: (static @"AboutBlockExplorer.png") <> "style" =: "width: 100%;") blank

  el "div" $ do
    text "1. The name of the network that is currently displaying block explorer data"
    el "br" blank
    text "2. Search the Code or Request Key fields of any transaction in the blockchain"
    el "br" blank
    text "3. Estimate of total network hash rate"
    el "br" blank
    text "4. Estimate of total network mining difficulty"
    el "br" blank
    text "5. The total number of transactions mined in the blockchain since genesis"
    el "br" blank
    text "6. Estimate of total coins circulating in the blockchain"
    el "br" blank
    text "7. Block Height indicates the length of the blockchain"
    el "br" blank
    text "8. The expected number of hashes to mine a block on this chain"
    el "br" blank
    text "9. Visual representation of the cryptographic link between a block and its neighbor blocks"
    el "br" blank
    text "10. The first few hex characters of the hash of the block header"
    el "br" blank
    text "11. How long ago the block was mined"
    el "br" blank
    text "12. The number of transactions in the block"

  el "h2" $ text "How to find transactions and explore blocks"
  el "p" $ text "The front page of the block explorer only shows recent network activity. However, there are many ways to search the entire blockchain."
  el "p" $ do
    el "b" $ text "Request Key: "
    text "Enter a transaction’s Request Key in the search bar. These are often provided by crypto wallets as a means to confirm that the transaction is in the blockchain."

  el "p" $ do
    el "b" $ text "Code: "
    text "In the search bar, toggle the dropdown from Request Key to Code. Use this to return a list of all transactions whose code appears in your query."

  el "p" $ text "Code search examples:"

  el "ul" $ do
    el "li" $ text "Enter an account name to return transfers in which it has participated — i.e. \"8cc8da8ea7cbfaf6510dbc5f402b025dceaa9a3eaf7145e0ba933d468b63d358\""
    el "li" $ text "Enter a function to return all top-level calls — i.e. \"coin.transfer-crosschain\""
    el "li" $ text "Enter \"module\" to return all transactions which deployed smart contracts"
    el "li" $ text "Enter \"189.6\" to return all transactions which transferred that amount of KDA"

  el "p" $ do
    el "b" $ text "Block: "
    text "Search for a specific block by entering its Chain ID and Block Height into the following URL format: https://explorer.chainweb.com/mainnet/chain/[X]/height/[Y] — replace [X] with Chain ID, replace [Y] with Block Height"

  el "h2" $ text "Why Kadena has multiple chains"
  el "p" $ text "The Kadena Public Blockchain runs a protocol called Chainweb, a novel parallel-chain Proof of Work architecture comprised of braided chains that all mine the same native currency and transfer liquidity between each other."
  el "p" $ text "In Chainweb, the braided chains incorporate Merkle proofs from adjacent chains in a fixed graph layout that ensures that proofs quickly propagate to every other chain in the system within some maximum block depth."
  el "p" $ text "Hover your cursor over any block to highlight the neighbor chains that are cryptographically linked to it. In this 10 chain configuration, notice that all 10 chains become braided together within the height of two blocks."
  el "p" $ text "By linking multiple chains together in this manner, Kadena offers massive throughput and enhanced security."
