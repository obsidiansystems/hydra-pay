-- | Utils that should really be part of cardano-api

module Cardano.Api.Extras where

import qualified Cardano.Api as Api
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

txOutToTxOutValue :: Api.TxOut ctx era -> Api.TxOutValue era
txOutToTxOutValue (Api.TxOut _ v _ _) = v

utxoValue :: Api.UTxO era -> Api.Value
utxoValue (Api.UTxO m) = foldMap (Api.txOutValueToValue . txOutToTxOutValue) $ Map.elems m

showLovelaceAsAda :: Api.Lovelace -> Text
showLovelaceAsAda l =
  let (whole, decimals) = showLovelaceAsAda' l
  in whole <> "." <> decimals

showLovelaceAsAda' :: Api.Lovelace -> (Text, Text)
showLovelaceAsAda' l = (showWithCommas whole, T.pack (show dec))
  where
    decimals :: Int
    decimals = 6

    whole, dec :: Integer
    (whole, dec) = fromIntegral l `quotRem` (10^decimals)

    showWithCommas :: Integer -> Text
    showWithCommas = T.reverse . T.intercalate "," . T.chunksOf 3 . T.reverse . T.pack . show

unsafeToAddressAny :: Text -> Api.AddressAny
unsafeToAddressAny txt =
  case Api.deserialiseAddress Api.AsAddressAny txt of
    Nothing -> error $ "Could not deserialise address: " <> show txt
    Just addr -> addr
