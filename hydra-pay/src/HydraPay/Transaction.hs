{-# LANGUAGE RecordWildCards #-}
-- |

module HydraPay.Transaction where

import Control.Exception (catch)
import Control.Lens ((^.), to)
import Control.Monad
import Control.Monad.IO.Class
import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import Cardano.Transaction hiding (TxId)
import qualified Data.Aeson as Aeson
import Data.Int (Int32)
import Data.String (fromString)
import qualified Data.Text as T
import System.Directory
import System.IO
import System.IO.Temp

import HydraPay.Cardano.Node
import HydraPay.Types (TxId(..))

-- | cardano-cli needs the params in a file, so we just create a temp file we can use for that purpose
withProtocolParamsFile :: Api.ProtocolParameters -> (FilePath -> IO a) -> IO a
withProtocolParamsFile pparams action = do
  createDirectoryIfMissing True tempTxDir
  withTempFile tempTxDir "params" $ \paramsPath handle -> do
    hClose handle
    Aeson.encodeFile paramsPath pparams
    action paramsPath

fanoutToL1Address :: (MonadIO m, HasNodeInfo a) => a -> Api.ProtocolParameters -> Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> m TxId
fanoutToL1Address a pparams fromAddr skPath toAddr amount = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let testnetMagic = Just 2 -- Preview network
        cfg = mkEvalConfig a socketPath paramsPath
    fmap (TxId . T.pack) $
      eval cfg (fanoutToL1AddressTx fromAddr skPath toAddr amount) `catch` \e@(EvalException _ _ _) -> print e >> pure "FAKE TX ID"
  where
    socketPath = a ^. nodeInfo . nodeInfo_socketPath

fanoutToL1AddressTx :: Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> Tx ()
fanoutToL1AddressTx fromAddr skPath toAddr lovelace = do
  Output {..} <- output (addressString toAddr) (fromString $ show lovelace <> " lovelace")
  void $ selectInputs oValue fromStr
  changeAddress fromStr
  void $ balanceNonAdaAssets fromStr
  sign skPath
  where
    fromStr = addressString fromAddr

tempTxDir :: FilePath
tempTxDir = "tx"

-- | Given node information, and a path to the node socket and protocol parameters create an EvalConfig for running the Tx monad
mkEvalConfig :: HasNodeInfo a => a -> FilePath -> FilePath -> EvalConfig
mkEvalConfig a nsFp ppFp = EvalConfig Nothing (ni ^. nodeInfo_magic . to (Just . fromIntegral)) (Just ppFp) False (Just nsFp)
  where
    ni = a ^. nodeInfo

addressString :: Api.AddressAny -> String
addressString = T.unpack . Api.serialiseAddress
