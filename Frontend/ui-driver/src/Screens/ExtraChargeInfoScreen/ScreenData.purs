module Screens.ExtraChargeInfoScreen.ScreenData where

import Prelude


type ExtraChargeInfoScreenState = {
    optionOpened :: Array Boolean,
    sheetPositionRef :: Number
}


initData :: ExtraChargeInfoScreenState
initData = {
    optionOpened: [true, false],
    sheetPositionRef : 1.0
}
