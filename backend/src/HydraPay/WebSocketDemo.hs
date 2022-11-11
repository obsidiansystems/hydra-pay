-- | 

module HydraPay.WebSocketDemo where

import Common.Helpers

import Hydra.Types
import Hydra.ClientInput
import Hydra.ServerOutput
import Hydra.Devnet

import HydraPay

import qualified Data.Text as T
import Data.IORef
import Data.Foldable
import Data.Aeson as Aeson
import GHC.Generics

import Control.Monad (forever, when)
import Control.Monad.Loops (untilJust)

import qualified Network.WebSockets as WS
import HydraPay.Api
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Interpolate ( i )
import HydraPay.WebSocket
import Control.Concurrent (threadDelay)
import CardanoNodeInfo
import Common.DemoApi

handleClientMessage :: State -> CardanoNodeInfo -> [(KeyPair, Address)] -> DClientMsg -> IO DServerMsg
handleClientMessage state cninf participants = \case
  DClientHello -> pure DServerHello
  DDemoInit -> demoFundInit state cninf participants
  DCloseFanout -> demoCloseFanout participants
  _ -> pure DUnhandledMessage


demoFundInit ::
  MonadIO m =>
  State ->
  CardanoNodeInfo ->
  [(KeyPair, Address)] ->
  m DServerMsg
demoFundInit state cninf (participants@[(ks1, addr1), (ks2, addr2)]) = do
  let addrs = snd <$> participants
  fmap (maybe DUnhandledMessage id) $ liftIO $ runHydraPayClient $ do
    let
      funds = ada 300

    for_ participants $ \(ks,addr) -> do
      let sk = _signingKey ks
      FundsTx tx <- requestResponse $ GetAddTx Funds addr funds
      liftIO $ signAndSubmitTx cninf sk tx

      FuelAmount amount <- requestResponse $ CheckFuel addr

      when (amount < ada 30) $ do
        FundsTx fueltx <- requestResponse $ GetAddTx Fuel addr (ada 100)
        liftIO $ signAndSubmitTx cninf sk fueltx
    HeadExistsResult exists <- requestResponse $ DoesHeadExist "demo"
    when exists $ do
      liftIO $ putStrLn "HEADEXISTS"
      OperationSuccess <- requestResponse $ TearDownHead "demo"
      pure ()
    liftIO $ putStrLn "CREATEHEAD"
    OperationSuccess <- requestResponse $ CreateHead $ HeadCreate "demo" addrs
    liftIO $ do
      threadDelay (seconds 5)
      putStrLn "INITHEAD"
    OperationSuccess <- requestResponse $ InitHead $ HeadInit "demo" addr1 3
    liftIO $ do
      threadDelay (seconds 5)
      -- Event waiting for Head Init to finish isn't enough, so we retry commits until success
      putStrLn "STARTING COMMITTING"
    for_ addrs $ \addr -> do
      let
        -- The delay here lets us have some falloff to avoid thrashing the nodes
        commitUntilSuccess delay = do
          liftIO $ putStrLn [i|Committing #{addr}|]
          result <- requestResponse $ CommitHead $ HeadCommit "demo" addr funds
          liftIO $ putStrLn [i|Result #{result}|]
          case result of
            (ServerError NodeCommandFailed) -> do
              liftIO $ threadDelay delay
              commitUntilSuccess $ delay * 2
            OperationSuccess -> pure ()
      commitUntilSuccess (seconds 30)
    liftIO $ putStrLn "WAITING FOR OPEN"
    -- TODO: waiting on confirmed head open status should be done via the HydraPay WS API
    waitForHeadStatus state "demo" Status_Open
    pure DInitDone


demoCloseFanout :: [(a, Address)] -> IO DServerMsg
demoCloseFanout (participants@[(ks1, addr1), (ks2, addr2)]) =
  fmap (maybe DUnhandledMessage id) $ runHydraPayClient $ do
    -- Close the head, wait for fanout!
    OperationSuccess <- requestResponse $ CloseHead "demo"

    -- Move funds from proxy addresses back to host addresses
    res <- requestResponse $ Withdraw addr1
    liftIO $ putStrLn $ "Withdraw addr1 result: " ++ show res
    res <- requestResponse $ Withdraw addr2
    liftIO $ putStrLn $ "Withdraw addr2 result: " ++ show res
    pure DCloseFanoutDone
