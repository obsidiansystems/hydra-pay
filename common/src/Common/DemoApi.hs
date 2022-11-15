module Common.DemoApi where
import Data.Aeson
import GHC.Generics


data DClientMsg
  = DClientHello
  | DDemoInit
  | DCloseFanout
  deriving (Eq, Show, Generic)

instance ToJSON DClientMsg
instance FromJSON DClientMsg



data DServerMsg
  = DServerHello
  | DUnhandledMessage
  | DInitDone
  | DCloseFanoutDone
  deriving (Eq, Show, Generic)

instance ToJSON DServerMsg
instance FromJSON DServerMsg

