-- |

module HydraPay.Worker where

import Control.Lens (iso)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.DB.Beam.Types (WrapColumnar(..))
import Rhyolite.Task.Beam

import HydraPay.Database.Workers


paymentChannelTask :: Task Postgres PaymentChannelTaskT (WrapColumnar PaymentChannelReq) Text (WrapColumnar (Maybe Bool))
paymentChannelTask = Task
  { _task_filter = const $ val_ True
  , _task_payload = WrapColumnar . _paymentChannelTask_payload
  , _task_checkedOutBy = paymentChannelTask_checkedOutBy
  , _task_hasRun = paymentChannelTask_finished
  , _task_result = paymentChannelTask_status . iso WrapColumnar unWrapColumnar
  }

-- openChannelTask :: Task Postgres PaymentChannelTaskT (WrapColumnar PaymentChannelReq) Text (WrapColumnar (Maybe Bool))
-- openChannelTask = Task
--   { _task_filter = \a -> case _paymentChannelTask_payload a of
--       PaymentChannelReq_Init _ -> val_ True
--       _ -> False
--   , _task_payload = WrapColumnar . _paymentChannelTask_payload
--   , _task_checkedOutBy = paymentChannelTask_checkedOutBy
--   , _task_hasRun = paymentChannelTask_finished
--   , _task_result = paymentChannelTask_status . iso WrapColumnar unWrapColumnar
--   }
