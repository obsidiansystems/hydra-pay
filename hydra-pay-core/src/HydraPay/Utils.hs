-- |

module HydraPay.Utils where

import Debug.Trace
import Control.Applicative
import Data.Aeson as Aeson
import Data.Text (Text, pack)
import System.Exit
import System.Process
import Control.Monad.IO.Class
import Control.Concurrent.STM
import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Shelley
import qualified Cardano.Ledger.BaseTypes as Ledger

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

tShow :: Show a => a -> Text
tShow = pack . show

eitherReadProcess :: MonadIO m => CreateProcess -> m (Either String String)
eitherReadProcess cp = do
  (code, out, err) <- liftIO $ readCreateProcessWithExitCode cp ""
  case code of
    ExitSuccess -> pure $ Right out
    ExitFailure _ -> pure $ Left err

withTMVar :: MonadIO m => TMVar a -> (a -> m (a, b)) -> m b
withTMVar var action = do
  val <- liftIO $ atomically $ takeTMVar var
  (a, b) <- action val
  liftIO $ atomically $ putTMVar var a
  pure b

withTMVar_ :: MonadIO m => TMVar a -> (a -> m ()) -> m ()
withTMVar_ var action = do
  withTMVar var $ \a -> do
    action a
    pure (a, ())

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b)  = Right b
maybeToEither a _ = Left a

addressNetwork :: Api.AddressAny -> Maybe Ledger.Network
addressNetwork = \case
  Api.AddressShelley (Shelley.ShelleyAddress network _ _) -> Just network
  Api.AddressByron _ -> Nothing

isTestnetAddress :: Api.AddressAny -> Bool
isTestnetAddress = maybe False (== Ledger.Testnet) . addressNetwork

findUTxOWithExactly :: Api.Lovelace -> Api.UTxO Api.BabbageEra -> Maybe (Api.UTxO Api.BabbageEra)
findUTxOWithExactly target utxos =
  case Map.size exactMatchesOnly of
    -- If we have exactly one, we have a match
    1 -> Just $ Api.UTxO exactMatchesOnly
    -- In any other case we were unable to find such a utxo
    _ -> Nothing
  where
    exactMatchesOnly = filterForExact utxos
    filterForExact = Map.take 1 . Map.filter (\x -> txOutValue x == target) . Api.unUTxO

ada :: Integer -> Api.Lovelace
ada x = fromIntegral $ x * 1000000

txOutValue :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> Api.Lovelace
txOutValue (Api.TxOut _ (Api.TxOutValue _ value) _ _) = Api.selectLovelace value
txOutValue _ = 0

totalLovelaceWithoutFuel :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalLovelaceWithoutFuel = sum . fmap (txOutValue) . filter (not . txOutIsFuel) . Map.elems . Api.unUTxO

totalLovelace :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalLovelace = sum . fmap (txOutValue) . Map.elems . Api.unUTxO

totalFuelLovelace :: Api.UTxO Api.BabbageEra -> Api.Lovelace
totalFuelLovelace = sum . fmap (txOutValue) . filter txOutIsFuel . Map.elems . Api.unUTxO

fuelMarkerDatumHash :: BS.ByteString
fuelMarkerDatumHash =
  "a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3"

txOutDatumIsFuel :: Api.TxOutDatum ctx era -> Bool
txOutDatumIsFuel (Api.TxOutDatumHash _ datumHash) =
  Aeson.encode datumHash == LBS.fromStrict ("\"" <> fuelMarkerDatumHash <> "\"")
txOutDatumIsFuel _ = False

txOutIsFuel :: Api.TxOut Api.CtxUTxO Api.BabbageEra -> Bool
txOutIsFuel (Api.TxOut _ _ datum _) = txOutDatumIsFuel datum

fuelIsPresent :: Api.UTxO Api.BabbageEra -> Bool
fuelIsPresent = not . Map.null . Map.filter (txOutIsFuel) . Api.unUTxO

firstNonFuelUTxO :: Api.UTxO Api.BabbageEra -> Maybe (Api.UTxO Api.BabbageEra)
firstNonFuelUTxO utxo = result
  where
    result = Map.foldrWithKey getValue Nothing nonFuelUtxos
    nonFuelUtxos = Map.filter (not . txOutIsFuel) $ Api.unUTxO utxo
    getValue k v b =
      b <|> (Just $ Api.UTxO $ Map.singleton k v)

mkCommitUTxOWithExactly :: Api.Lovelace -> Api.UTxO Api.BabbageEra -> Maybe (Api.UTxO Api.BabbageEra)
mkCommitUTxOWithExactly wantedAmount utxo =
  trace ("Initial value: " <> show utxo) $ trace ("Final value: " <> show result) result
  where
    result = Map.foldrWithKey getValue Nothing nonFuelUtxos
    nonFuelUtxos = Map.filter (not . txOutIsFuel) $ Api.unUTxO utxo
    getValue k v b =
      b <|> (case txOutValue v == wantedAmount of
        True -> trace "We found one!" $ Just $ Api.UTxO $ Map.singleton k v
        False -> trace "We casn't use this one" $ Nothing)
