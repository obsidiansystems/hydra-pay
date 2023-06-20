-- |

module HydraPay.Worker where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.DB.Beam.Types (WrapColumnar(..))
import Rhyolite.Task.Beam

import HydraPay.Database.Workers


paymentChannelTask :: TaskWithoutHasRun Postgres PaymentChannelTaskT (WrapColumnar PaymentChannelReq) Text (WrapColumnar (Maybe Bool))
paymentChannelTask = TaskWithoutHasRun
  { _taskWithoutHasRun_filter = \chan -> _paymentChannelTask_status chan /=. (val_ $ Just True)

  , _taskWithoutHasRun_payload = WrapColumnar . _paymentChannelTask_payload
  , _taskWithoutHasRun_checkedOutBy = _paymentChannelTask_checkedOutBy
  , _taskWithoutHasRun_result = WrapColumnar . _paymentChannelTask_status
  }
