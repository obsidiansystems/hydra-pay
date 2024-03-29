{-# LANGUAGE RecordWildCards #-}
-- |

module HydraPay.Transaction where

import HydraPay.Types
import Control.Lens ((^.), to)
import Control.Monad
import Control.Monad.IO.Class
import qualified Cardano.Api as Api
import Cardano.Transaction hiding (TxId)
import Cardano.Transaction.Eval (evalEither)
import Data.Int (Int32)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.IO
import System.IO.Temp

import HydraPay.Cardano.Node

-- | cardano-cli needs the params in a file, so we just create a temp file we can use for that purpose
withProtocolParamsFile :: T.Text -> (FilePath -> IO a) -> IO a
withProtocolParamsFile pparams action = do
  createDirectoryIfMissing True tempTxDir
  withTempFile tempTxDir "params" $ \paramsPath handle -> do
    hClose handle
    T.writeFile paramsPath pparams
    action paramsPath

fanoutToL1Address :: (MonadIO m, HasNodeInfo a) => a -> T.Text -> Api.AddressAny -> FilePath -> Api.AddressAny -> Int32 -> m (Either Text TxId)
fanoutToL1Address a pparams fromAddr skPath toAddr amount = do
  liftIO $ withProtocolParamsFile pparams $ \paramsPath -> do
    let cfg = mkEvalConfig a paramsPath
    (fmap . fmap) (TxId . T.pack) $
      evalEither cfg (fanoutToL1AddressTx fromAddr skPath toAddr amount)

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

-- | Given node information and a path to the protocol parameters create an EvalConfig for running the Tx monad
mkEvalConfig :: HasNodeInfo a => a -> FilePath -> EvalConfig
mkEvalConfig a ppFp = EvalConfig Nothing (ni ^. nodeInfo_magic . to (Just . fromIntegral)) (Just ppFp) False (Just (ni ^. nodeInfo_socketPath))
  where
    ni = a ^. nodeInfo

addressString :: ToAddress a => a -> String
addressString = T.unpack . Api.serialiseAddress . toAddress
