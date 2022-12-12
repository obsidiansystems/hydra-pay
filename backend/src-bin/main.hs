import Backend
import Frontend
import Obelisk.Backend
import Snap.Internal.Http.Server.Config
import Config
import Data.ByteString.UTF8 as BSU

main :: IO ()
main =
  let backendConfig conf' =
        BackendConfig
        (runSnapWithConfig conf')
        defaultStaticAssets
        defaultGhcjsWidgets
  in do
    cfg <- getHydraCLIConfig
    runBackendWith (backendConfig ((setPort (_port cfg) $ setBind (BSU.fromString . _bind $ cfg) defaultConfig))) backend frontend
