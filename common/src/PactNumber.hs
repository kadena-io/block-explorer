{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module PactNumber where

------------------------------------------------------------------------------
import qualified Chainweb.Api.ParsedNumbers as PN

import           Control.Applicative
import qualified Data.Aeson as A
import           Data.Aeson
import qualified Data.Aeson.Types as A
import           Data.Decimal
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------------------------

data PactNumber = PactInteger Integer | PactDecimal Decimal
  deriving (Eq,Ord,Show)

instance FromJSON PactNumber where
  parseJSON v@(A.Number s) =
    case floatingOrInteger s of
      Left _ -> PactDecimal <$> PN.decoder PN.decimalCodec v
      Right _ -> PactInteger <$> PN.decoder PN.integerCodec v
  parseJSON v@(Object _)
      = PactInteger <$> PN.decoder PN.integerCodec v
    <|> PactDecimal <$> PN.decoder PN.decimalCodec v
  parseJSON v = A.typeMismatch "Numeric" v

pactNumberToDecimal :: PactNumber -> Decimal
pactNumberToDecimal (PactInteger i) = fromIntegral i
pactNumberToDecimal (PactDecimal d) = d

pactNumberToText :: PactNumber -> Text
pactNumberToText (PactInteger i) = T.pack $ show i
pactNumberToText (PactDecimal d) = T.pack $ show d
