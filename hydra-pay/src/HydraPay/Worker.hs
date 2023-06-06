-- |

module HydraPay.Worker where

import Control.Lens (iso)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.DB.Beam.Types (WrapColumnar(..))
import Rhyolite.Task.Beam

import HydraPay.Database.Workers


openChannelTask :: Task Postgres OpenChannelTaskT (WrapColumnar OpenChannelReq) Text (WrapColumnar (Maybe Bool))
openChannelTask = Task
  { _task_filter = const $ val_ True
  , _task_payload = WrapColumnar . _openChannelTask_payload
  , _task_checkedOutBy = openChannelTask_checkedOutBy
  , _task_hasRun = openChannelTask_finished
  , _task_result = openChannelTask_status . iso WrapColumnar unWrapColumnar
  }
