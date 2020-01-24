{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Page.Common where


------------------------------------------------------------------------------
import Control.Lens
------------------------------------------------------------------------------
import Data.Aeson
import qualified Data.Text as T
import Data.Foldable
import Data.Text (Text)
------------------------------------------------------------------------------
import Common.Utils

unwrapJSON :: Value -> Text
unwrapJSON = \case
    Object o -> "{ " <> (T.intercalate " , " (toList $ imap keyvalue o)) <> " }"
    Array a -> "[" <> (T.intercalate "," (toList $ fmap unwrapJSON a) ) <> "]"
    String s -> s
    Null -> "null"
    Number n -> tshow n
    Bool b  -> if b then "true" else "false"
  where
    keyvalue k v = k <> " : " <> unwrapJSON v
