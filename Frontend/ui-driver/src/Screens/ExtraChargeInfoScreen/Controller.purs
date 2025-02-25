module Screens.ExtraChargeInfoScreen.Controller where

import Prelude
import Effect (Effect)
import PrestoDOM (Eval, update, Props, exit, continue)
import PrestoDOM.Types.Core (class Loggable,defaultPerformLog)
import Screens.Types
import Log (trackAppActionClick, trackAppScreenRender, trackAppEndScreen)
import Engineering.Helpers.LogEvent (logEvent)
import Effect.Unsafe (unsafePerformEffect)
import Screens.ExtraChargeInfoScreen.ScreenData


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
    performLog = defaultPerformLog

data ScreenOutput = NoOutput

data Action = NoAction

eval :: Action -> ExtraChargeInfoScreenState -> Eval Action ScreenOutput ExtraChargeInfoScreenState
eval NoAction state = update state
