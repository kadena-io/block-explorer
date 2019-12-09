{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.Payload where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
------------------------------------------------------------------------------

data Exec = Exec
  { _exec_code :: Text
  , _exec_data :: Maybe Object
  } deriving (Eq,Show)

instance FromJSON Exec where
  parseJSON = withObject "Exec" $ \o -> Exec
    <$> o .: "code"
    <*> o .: "data"

data Cont = Cont
  { _cont_pactId :: Text
  , _cont_rollback :: Bool
  , _cont_step :: Int
  , _cont_data :: Object
  , _cont_proof :: Text
  } deriving (Eq,Show)

instance FromJSON Cont where
  parseJSON = withObject "Cont" $ \o -> Cont
    <$> o .: "pactId"
    <*> o .: "rollback"
    <*> o .: "step"
    <*> o .: "data"
    <*> o .: "proof"

data Payload = ExecPayload Exec | ContPayload Cont
  deriving (Eq,Show)

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o -> do
    case HM.lookup "exec" o of
      Nothing -> case HM.lookup "cont" o of
                   Nothing -> fail "Payload must be exec or cont"
                   Just v -> ContPayload <$> parseJSON v
      Just v -> ExecPayload <$> parseJSON v

payloadCode :: Payload -> Text
payloadCode (ExecPayload e) = _exec_code e
payloadCode (ContPayload c) = _cont_pactId c
