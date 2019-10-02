{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Page.Transaction where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Aeson
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord
import           Data.Readable
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHCJS.DOM.Types (MonadJSM)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (Value)
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Route
import           Common.Utils
import           Frontend.App
import           Frontend.AppState
import           Frontend.ChainwebApi
import           Frontend.Common
------------------------------------------------------------------------------

transactionPage
  :: (MonadApp r t m)
  => BlockPayload
  -> m ()
transactionPage bp = do
  el "h2" $ text "Transaction"
  divClass "ui accordion" $ do
    forM_ (_blockPayload_transactions bp) $ \t -> mdo
      open <- toggle False $ domEvent Click e
      let addActive cls active =
            ("class" =: if active then ("active " <> cls) else cls)
      (e,_) <- elDynAttr' "div" (addActive "title" <$> open) $ do
        elClass "i" "dropdown icon" blank
        el "code" $ text $ payloadCode $ _pactCommand_payload $ _transaction_cmd t
      elDynAttr "div" (addActive "content" <$> open) $ do
        elClass "table" "ui definition table" $ do
          el "tbody" $ do
            tfield "Transaction Hash" $ text $ hashHex $ _transaction_hash t
            tfield "Meta" $ do
              let meta = _pactCommand_meta $ _transaction_cmd t
              elClass "table" "ui definition table" $ el "tbody" $ do
                tfield "Chain" $ text $ _chainwebMeta_chainId meta
                tfield "Sender" $ text $ _chainwebMeta_sender meta
                tfield "Gas Price" $ text $ tshow $ _chainwebMeta_gasPrice meta
                tfield "Gas Limit" $ text $ tshow $ _chainwebMeta_gasLimit meta
                tfield "TTL" $ text $ tshow $ _chainwebMeta_ttl meta
                tfield "Creation Time" $ text $ tshow $ _chainwebMeta_creationTime meta
            tfield "Signers" $ do
              forM_ (_pactCommand_signers $ _transaction_cmd t) $ \s -> do
                elClass "table" "ui definition table" $ el "tbody" $ do
                  tfield "Account" $ text $ _signer_addr s
                  tfield "Public Key" $ text $ _signer_pubKey s
                  tfield "Scheme" $ text $ _signer_scheme s
            tfield "Signatures" $ do
              forM_ (_transaction_sigs t) $ \s -> do
                el "div" $ text $ unSig s
