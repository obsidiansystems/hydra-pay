-- | Some helpers to provide conversion from/to cardano-api to
-- cardano-transaction-builder data types.

module Cardano.Transaction.CardanoApi where

import Cardano.Transaction (UTxO(..))
import Data.Map (Map)
import qualified Cardano.Api as Api
import qualified Cardano.Transaction as Tx
import qualified Data.Map as Map
import qualified Data.Text as T

fromCardanoApiUTxO :: Api.UTxO Api.BabbageEra -> [Tx.UTxO]
fromCardanoApiUTxO (Api.UTxO m) = Map.elems $ Map.mapWithKey go m
  where
    go (Api.TxIn txId (Api.TxIx txIndex)) (Api.TxOut _ val _ _) =
      Tx.UTxO
        { utxoIndex = fromIntegral txIndex
        , utxoTx = T.unpack $ Api.serialiseToRawBytesHexText txId
        , utxoValue = fromCardanoApiValue $ Api.txOutValueToValue val
        , utxoDatum = Tx.UTxO_NoDatum
        }

fromCardanoApiValue :: Api.Value -> Tx.Value
fromCardanoApiValue v = Tx.Value $ Map.unionsWith (Map.unionWith (+)) $ uncurry fromValueList <$> valList
  where
    fromValueList :: Api.AssetId -> Api.Quantity -> Map String (Map String Integer)
    fromValueList a (Api.Quantity q) = case a of
      Api.AdaAssetId -> Map.singleton "" $ Map.singleton "" q
      Api.AssetId p n -> Map.singleton (show p) $ Map.singleton (show n) q
    valList = Api.valueToList v
