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
import           Control.Applicative
import qualified Data.Aeson as A
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson.Types as A
import           Data.Decimal
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)


------------------------------------------------------------------------------

data PactNumber = PactInteger Integer | PactDecimal Decimal
  deriving (Eq,Ord,Show)

instance FromJSON PactNumber where
  parseJSON v@(A.Number s) =
    case floatingOrInteger s of
      Left _ -> PactDecimal <$> decoder decimalCodec v
      Right _ -> PactInteger <$> decoder integerCodec v
  parseJSON v@(Object _) = do
    (PactInteger <$> decoder integerCodec v) <|> (PactDecimal <$> decoder decimalCodec v)
  parseJSON v = A.typeMismatch "Numeric" v

pactNumberToDecimal :: PactNumber -> Decimal
pactNumberToDecimal (PactInteger i) = fromIntegral i
pactNumberToDecimal (PactDecimal d) = d

pactNumberToText :: PactNumber -> Text
pactNumberToText (PactInteger i) = T.pack $ show i
pactNumberToText (PactDecimal d) = T.pack $ show d

------------------------------------------------------------------------------
-- The following code taken from Pact
------------------------------------------------------------------------------

-- | Min, max values that Javascript doesn't mess up.
--
--   http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html
--   "The integer part of the Number type in Javascript is safe in [-2^53 .. 2^53] (253 = 9 007 199 254 740 992).
--    Beyond this there will be precision loss on the least significant numbers."
jsIntegerBounds :: (Integer, Integer)
jsIntegerBounds = (-9007199254740991,9007199254740991)

isSafeInteger :: Integer -> Bool
isSafeInteger i = i >= l && i <= h
  where (l,h) = jsIntegerBounds

-- | JSON codec pair.
data Codec a = Codec {
  encoder :: a -> Value,
  decoder :: Value -> Parser a
  }

-- | Integers encode to an object that uses Number if in reasonable JS bounds or String otherwise.
integerCodec :: Codec Integer
integerCodec = Codec encodeInteger decodeInteger
  where
    encodeInteger i
      | isSafeInteger i = object [ field .= i ]
      | otherwise = object [ field .= show i ]
    {-# INLINE encodeInteger #-}
    decodeInteger = withObject "Integer" $ \o -> do
      s <- o .: field
      case s of
        Number n -> return (round n)
        String n -> case readMaybe (T.unpack n) of
          Just i -> return i
          Nothing -> fail $ "Invalid integer value: " ++ show s
        _ -> fail $ "Invalid integer value: " ++ show s
    {-# INLINE decodeInteger #-}
    field = "int"

-- | Decimals encode to a Scientific, which is encoded as an object + String
-- if mantissa precision exceeds JS.
-- TODO fromRational . toRational may not be the speediest.
decimalCodec :: Codec Decimal
decimalCodec = Codec enc dec
  where
    enc d@(Decimal _places mantissa)
      | isSafeInteger mantissa = Number $ fromRational $ toRational d
      | otherwise = object [ field .= show d ]
    {-# INLINE enc #-}
    dec (Number n) = return $ fromRational $ toRational n
    dec (A.Object o) = o .: field >>= \s -> case readMaybe (T.unpack s) of
      Just d -> return d
      Nothing -> fail $ "Invalid decimal value: " ++ show s
    dec v = fail $ "Invalid decimal value: " ++ show v
    {-# INLINE dec #-}
    field = "decimal"
