module HydraPay.Cardano.Hydra.RunningHead where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map (Map)
import System.IO
import System.Process

import HydraPay.Cardano.Hydra.Api
import HydraPay.PortRange
import qualified Cardano.Api as Api

-- | A running Hydra Head
data RunningHydraHead = RunningHydraHead
  { _hydraHead_status :: TVar HydraHeadStatus
  , _hydraHead_handles :: Map Api.AddressAny HydraNode
  }

data HydraHeadStatus
  = HydraHead_Uninitialized
  | HydraHead_Initializing
  | HydraHead_Open
  | HydraHead_Closed
  | HydraHead_Finalized
  | HydraHead_Aborted

type ProcessInfo = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data HydraNode = HydraNode
  { _hydraNode_apiPort :: Port
  , _hydraNode_processInfo :: TMVar ProcessInfo
  , _hydraNode_communicationThread :: TMVar ThreadId
  , _hydraNode_status :: TVar HydraNodeStatus
  , _hydraNode_runnerThread :: ThreadId
  , _hydraNode_inputs :: TBQueue ClientInput
  , _hydraNode_outputs :: TChan ServerOutput
  }

data HydraNodeStatus
  = HydraNodeStatus_Unavailable
  | HydraNodeStatus_Replaying
  | HydraNodeStatus_Replayed
  | HydraNodeStatus_PeersConnected
  | HydraNodeStatus_Closed -- ^ TODO do we need this? Should we be removing this node after it is close? But what about if we haven't fanned out yet
  deriving (Eq, Show)

data HydraNodeRequest = HydraNodeRequest
  { _hydraNodeRequest_id :: Int
  , _hydraNodeRequest_clientInput :: ClientInput
  , _hydraNodeRequest_mailbox :: TMVar ServerOutput
  }


