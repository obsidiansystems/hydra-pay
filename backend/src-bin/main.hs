import Backend
import Frontend
import Obelisk.Backend
import Snap.Internal.Http.Server.Config

main :: IO ()
main =
  let backendConfig conf' =
        BackendConfig
        (runSnapWithConfig conf')
        defaultStaticAssets
        defaultGhcjsWidgets
      conf = backendConfig defaultConfig
  in runBackendWith conf backend frontend
