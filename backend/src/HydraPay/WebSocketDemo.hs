-- | 

module HydraPay.WebSocketDemo where

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
                liftIO $ runHydraPayClient $ \conn -> do
                  let
                    funds = ada 300

                  for_ participants $ \(ks,addr) -> do
                    let sk = _signingKey ks
                    Just (FundsTx tx) <- requestResponse conn $ GetAddTx Funds addr funds
                    signAndSubmitTx cninf sk tx

                    Just (FuelAmount amount) <- requestResponse conn $ CheckFuel addr

                    when (amount < ada 30) $ do
                      Just (FundsTx fueltx) <- requestResponse conn $ GetAddTx Fuel addr (ada 100)
                      signAndSubmitTx cninf sk fueltx
                  Just (HeadExistsResult exists) <- requestResponse conn $ DoesHeadExist "demo"
                  when exists $ do
                    putStrLn "HEADEXISTS"
                    Just OperationSuccess <- requestResponse conn $ TearDownHead "demo"
                    pure ()
                  putStrLn "CREATEHEAD"
                  Just OperationSuccess <- requestResponse conn $ CreateHead $ HeadCreate "demo" addrs
                  threadDelay (seconds 5)
                  putStrLn "INITHEAD"
                  Just OperationSuccess <- requestResponse conn $ InitHead $ HeadInit "demo" addr1 3
                  threadDelay (seconds 5)
                  -- Event waiting for Head Init to finish isn't enough, so we retry commits until success
                  putStrLn "STARTING COMMITTING"
                  for_ addrs $ \addr -> do
                    let
                      -- The delay here lets us have some falloff to avoid thrashing the nodes
                      commitUntilSuccess delay = do
                        putStrLn [i|Committing #{addr}|]
                        result <- requestResponse conn $ CommitHead $ HeadCommit "demo" addr funds
                        putStrLn [i|Result #{result}|]
                        case result of
                          Just (ServerError NodeCommandFailed) -> do
                            threadDelay delay
                            commitUntilSuccess $ delay * 2
                          Just OperationSuccess -> pure ()
                    commitUntilSuccess (seconds 30)
                  putStrLn "WAITING FOR OPEN"
                  -- TODO: this should be done via the HydraPay WS API
--                  waitForHeadStatus state "demo" Status_Open
                  pure DInitDone


demoCloseFanout :: [(a, Address)] -> IO DServerMsg
demoCloseFanout (participants@[(ks1, addr1), (ks2, addr2)]) =
  runHydraPayClient $ \conn -> do
                  -- Close the head, wait for fanout!
                  Just OperationSuccess <- requestResponse conn $ CloseHead "demo"

                  -- Move funds from proxy addresses back to host addresses
                  res <- requestResponse conn $ Withdraw addr1
                  putStrLn $ "Withdraw addr1 result: " ++ show res
                  res <- requestResponse conn $ Withdraw addr2
                  putStrLn $ "Withdraw addr2 result: " ++ show res
                  pure DCloseFanoutDone


seconds :: Int -> Int
seconds = (* 1000000)
