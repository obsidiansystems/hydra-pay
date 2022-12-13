-- | 

module Hydra.Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (parseFail)

import Control.Applicative

import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Numeric.Natural (Natural)
import Data.Attoparsec.Text
import Data.Attoparsec.ByteString.Char8 (isDigit, isAlpha_ascii)

-- | Cardano address
newtype Address =
  UnsafeToAddress { unAddress :: T.Text }
  deriving (Ord, Eq, Show, Generic)

instance ToJSON Address where
  toJSON = String . unAddress

instance FromJSON Address where
  parseJSON = withText "Address" $ \t -> do
    case parseAddress t of
      Right addr -> pure addr
      Left err -> parseFail $ "Not a valid Cardano Address: " <> err

instance ToJSONKey Address
instance FromJSONKey Address

addressToString :: Address -> String
addressToString = T.unpack . unAddress

parseAddress :: T.Text -> Either String Address
parseAddress t =
  case parseOnly parser t of
    Left err -> Left err
    Right addr ->
      case addr == obviouslyInvalidAddress of
        True -> Left "This is Hydra Pay example cardano address, rejecting request"
        False -> Right addr
  where
    parser = do
      prefix <- string "addr1" <|> string "addr_test1"
      rest <- takeWhile1 (\x -> isDigit x || isAlpha_ascii x)
      endOfInput
      pure $ UnsafeToAddress $ prefix <> rest

obviouslyInvalidAddress :: Address
obviouslyInvalidAddress = UnsafeToAddress "addr_test1thisaddressisobviouslyinvaliddonotusethisaddressplease"

type Lovelace = Integer

type NodeId = T.Text

type TxIn = T.Text
type WholeUTXO = Map TxIn TxInInfo

ada :: Num a => a -> a
ada = (* 1000000)

lovelaceToAda :: Integer -> Float
lovelaceToAda n = fromIntegral n / 1000000

filterOutFuel :: WholeUTXO -> WholeUTXO
filterOutFuel = Map.filter (not . isFuel)

filterFuel :: WholeUTXO -> WholeUTXO
filterFuel = Map.filter (isFuel)

isFuel :: TxInInfo -> Bool
isFuel txinfo = datumhash txinfo == Just fuelMarkerDatumHash

data TxInInfo = TxInInfo
  { address :: Address
  , datumhash :: Maybe T.Text
  , value :: Map T.Text Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON TxInInfo
instance ToJSON TxInInfo

fuelMarkerDatumHash :: T.Text
fuelMarkerDatumHash = "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"


-- REVIEW(SN): This is also used in hydra-tui

-- Below various types copied/adapted from hydra-poc code

data Host = Host
  { hostname :: Text
  , port :: PortNumber
  }
  deriving (Ord, Generic, Eq, Show)

instance ToJSON Host
instance FromJSON Host


newtype Party = Party {vkey :: T.Text}
  deriving (Eq, Show, Read, Generic)

instance ToJSON Party
instance FromJSON Party

type UTxOType tx = Value
type MultiSignature x = Value
type PostChainTx tx = Value 
type PostTxError tx = Value
type ValidationError = Value
type SnapshotNumber = Natural
type PortNumber = Natural
type ContestationPeriod = Natural
