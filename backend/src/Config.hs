module Config where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)

data HydraPayConfig = HydraPayConfig
  { _hydraPayMode :: HydraPayMode
  , _port :: Int
  , _bind :: String
  }
  deriving (Show,Read)

-- | Configure Cardano (L1) and Hydra networks. This is either an
-- explicit configuration for Cardano Node and Hydra Nodes, or the
-- default mode which runs a devnet for live documentation.
data HydraPayMode
  = LiveDocMode
  | ConfiguredMode
    { _cardanoNodeParams :: CardanoNodeParams
    , _hydraNodeParams :: HydraNodeParams
    }
  deriving (Show,Read)

data CardanoNodeParams = CardanoNodeParams
  { _testnetMagic :: Int
  , _nodeSocket :: FilePath
  , _ledgerGenesis :: FilePath
  , _ledgerProtocolParameters :: FilePath
  }
  deriving (Show,Read)

data HydraNodeParams = HydraNodeParams
  { _hydraScriptsTxId :: String
  , _hydraLedgerProtocolParameters :: FilePath
  , _hydraLedgerGenesis :: FilePath
  }
  deriving (Show,Read)

-- | Unless specified otherwise the default is 'LiveDocMode'.
hydraPayConfigParser :: Parser HydraPayConfig
hydraPayConfigParser =
  HydraPayConfig
  <$> (fromMaybe LiveDocMode <$> optional netConfigParser)
  <*> (fromMaybe 8000 <$> optional (option auto (long "port" <> help "Port to use for the WebSocket endpoint and live documentation page")))
  <*> (fromMaybe "0.0.0.0" <$> optional (strOption (long "bind" <> help "Address or hostname to bind to")))

netConfigParser :: Parser HydraPayMode
netConfigParser =
  flag' LiveDocMode (long "live-docs" <> help "Provide a live documentation page which runs HydraPay on a Cardano Devnet")
  <|> (uncurry ConfiguredMode <$> nodeParamsParser)

nodeParamsParser :: Parser (CardanoNodeParams, HydraNodeParams)
nodeParamsParser =
  (,)
  <$>
  (CardanoNodeParams
   <$> option auto (long "testnet-magic")
   <*> strOption (long "node-socket")
   <*> strOption (long "ledger-genesis")
   <*> strOption (long "ledger-protocol-parameters"))
  <*>
  (HydraNodeParams
   <$> strOption (long "hydra-scripts-tx-id")
   <*> strOption (long "hydra-ledger-protocol-parameters")
   <*> strOption (long "hydra-ledger-genesis"))

getHydraCLIConfig :: IO HydraPayConfig
getHydraCLIConfig = execParser (info (hydraPayConfigParser <**> helper) fullDesc)
