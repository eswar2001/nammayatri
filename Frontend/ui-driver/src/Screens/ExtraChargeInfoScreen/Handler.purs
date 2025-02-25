module Screens.ExtraChargeInfoScreen.Handler where

import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core.Types.Language.Flow (showScreen)
import Presto.Core.Types.Language.Flow (Flow, doAff , getLogFields)
import Presto.Core.Types.Language.Flow (getState) as Flow
import Screens.ExtraChargeInfoScreen.Controller (ScreenOutput(..))
import Types.App (FlowBT, GlobalState(..), EXTRA_CHARGE_INFO_SCREEN(..))
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, runScreenWithNameSpace)
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Effect.Class (liftEffect)
import Screens.ExtraChargeInfoScreen.View as ExtraChargeInfoScreen
import Debug


extraChargeInfoScreen :: FlowBT String EXTRA_CHARGE_INFO_SCREEN
extraChargeInfoScreen = do
    let _  = spy "ExtraChargeInfoScreen" "inside extraChargeInfoScreen handler"
    (GlobalState state) <- getState
    void $ lift $ lift $ doAff $ liftEffect $ initUIWithNameSpace "ExtraChargeInfoScreen" Nothing
    let _  = spy "ExtraChargeInfoScreen" "after initUIWithNameSpace"
    act <- lift $ lift $ showScreenWithNameSpace $ ExtraChargeInfoScreen.screen state.extraChargeInfoScreen
    let _  = spy "ExtraChargeInfoScreen" "exit From runScreenWithNameSpace"
    void $ lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "ExtraChargeInfoScreen"
    App.BackT $ pure App.GoBack
