module ParseConfig where

import Options.Applicative
import Data.Maybe (fromMaybe)
import HydraPay.Config

-- | Obelisk's 'ob run' passes a "--quiet". This parser
-- transformer ignores "--quiet".
ignoreObeliskRunArgs :: Parser a -> Parser a
ignoreObeliskRunArgs p =
  (\_ a -> a) <$> switch (long "quiet" <> hidden) <*> p


-- | Unless specified otherwise the default is 'ManagedDevnet'.
hydraPayConfigParser :: Parser HydraPayConfig
hydraPayConfigParser =
  ignoreObeliskRunArgs
  $ HydraPayConfig
  <$> (fromMaybe ManagedDevnetMode <$> optional netConfigParser)
  <*> (fromMaybe 8000 <$> optional (option auto (long "port" <> help "Port to use for the WebSocket endpoint and live documentation page")))
  <*> (fromMaybe "0.0.0.0" <$> optional (strOption (long "bind" <> help "Address or hostname to bind to")))

netConfigParser :: Parser HydraPayMode
netConfigParser =
  flag' ManagedDevnetMode (long "manage-devnet" <> help "Have Hydra Pay start a Cardano devnet. The socket and configuration can be found in the livedoc-devnet directory while it's running.")
  <|> (uncurry ConfiguredMode <$> nodeParamsParser)

nodeParamsParser :: Parser (CardanoNodeParams, HydraNodeParams)
nodeParamsParser =
  (,)
  <$>
  (CardanoNodeParams
   <$> option auto (long "testnet-magic")
   <*> strOption (long "node-socket")
   <*> strOption (long "ledger-genesis"))
  <*>
  (HydraNodeParams
   <$> strOption (long "hydra-scripts-tx-id")
   <*> strOption (long "hydra-ledger-protocol-parameters")
   <*> strOption (long "hydra-ledger-genesis"))

getHydraCLIConfig :: IO HydraPayConfig
getHydraCLIConfig = execParser (info (hydraPayConfigParser <**> helper) fullDesc)
