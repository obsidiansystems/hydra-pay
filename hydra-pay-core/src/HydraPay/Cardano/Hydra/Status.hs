-- |

module HydraPay.Cardano.Hydra.Status where

import Data.Aeson
import GHC.Generics

data HydraHeadStatus
  = HydraHeadStatus_Unknown
  | HydraHeadStatus_Created
  | HydraHeadStatus_Initialized
  | HydraHeadStatus_Open
  | HydraHeadStatus_Closed
  | HydraHeadStatus_Finalized
  | HydraHeadStatus_Error
  | HydraHeadStatus_Done
  deriving (Show, Eq, Ord, Read, Generic, Enum)

instance ToJSON HydraHeadStatus
instance FromJSON HydraHeadStatus
