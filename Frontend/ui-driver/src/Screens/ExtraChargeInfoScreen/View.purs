module Screens.ExtraChargeInfoScreen.View where

import Prelude
import PrestoDOM
import Screens.Types
import Data.Maybe
import Effect
import Language.Types
import Common.Types.App
import PrestoDOM.Types.DomAttributes
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Screens.ExtraChargeInfoScreen.Controller
import Engineering.Helpers.Commons as EHC
import Language.Strings
import JBridge as JB
import Screens.ExtraChargeInfoScreen.ScreenData
import Engineering.Helpers.Commons
import Debug

screen :: ExtraChargeInfoScreenState -> ScopedScreen Action ExtraChargeInfoScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Just "ExtraChargeInfoScreen"
  , name : "ExtraChargeInfoScreen"
  , globalEvents : []
  , eval
  }


view ::forall w .  (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let
    _  = spy "ExtraChargeInfoScreen" "inside View"
  in
    linearLayout[
        height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.transparentMid
      , alpha 0.1
      , orientation VERTICAL
      , gravity BOTTOM
    ][
      linearLayout[
        height $ V $ (screenHeight unit) - 200
      , width MATCH_PARENT
      , background Color.white900
      ][
        textView[
          text "Hello Work"
        ]
      ]
    ]
