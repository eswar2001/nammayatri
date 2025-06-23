{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionScreen.Controller where

import Components.ErrorModal.Controller as ErrorModalController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PopUpModal as PopUpModal
import Effect.Uncurried (EffectFn3, mkEffectFn3, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import JBridge (firebaseLogEvent, isInternetAvailable, requestLocation, getLocationPermissionStatus,isLocationEnabled,askRequestedPermissionsWithCallback, isLocationEnabledWithoutEff,isLocationPermissionEnabled,isLocationPermissionEnabledWithoutEff)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, Unit, bind, discard, pure, unit, ($), (==), (&&), (||),not)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (PermissionScreenState, PermissionScreenStage(..))
import Effect.Unsafe 
import Engineering.Helpers.LogEvent (logEvent)
import Data.Array
import PrestoDOM.Core (getPushFn)
import Debug (spy)
import Helpers.Utils (launchAppSettings)
import Data.Maybe (Maybe(..))
instance showAction :: Show Action where 
    show _ = ""
  
instance loggableAction :: Loggable Action where 
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen PERMISSION_SCREEN)
      BackPressed -> do
        trackAppBackPress appId (getScreen PERMISSION_SCREEN)
        trackAppEndScreen appId (getScreen PERMISSION_SCREEN)
      ErrorModalActionController act -> case act of
        ErrorModalController.PrimaryButtonActionController act -> case act of 
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "error_modal_action" "primary_button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "error_modal_action" "primary_button_no_action"
      PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "primary_button" "grant_access"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen PERMISSION_SCREEN) "primary_button" "no_action"
      Reload -> do
        trackAppActionClick appId (getScreen PERMISSION_SCREEN) "in_screen" "reload"
        trackAppEndScreen appId (getScreen PERMISSION_SCREEN)
      InternetCallBackCustomer str -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "internet_call_back_customer"
      LocationPermissionCallBackCustomer str -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "location_permission_call_back_customer"
      RequestLocation -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "request_location"
      NoAction -> trackAppScreenEvent appId (getScreen PERMISSION_SCREEN) "in_screen" "no_action"
      _ -> pure unit

data Action = ErrorModalActionController ErrorModalController.Action 
            | PrimaryButtonActionController PrimaryButtonController.Action
            | NoAction
            | Reload
            | BackPressed
            | LocationPermissionCallBackCustomer Boolean
            | InternetCallBackCustomer String
            | AfterRender
            | RequestLocation
            | LocationBlockerPopUpAC PopUpModal.Action
            | UpdateState PermissionScreenState

data ScreenOutput = GoBack | Refresh | InternetCallBack PermissionScreenState | LocationCallBack PermissionScreenState 

eval :: Action -> PermissionScreenState -> Eval Action ScreenOutput PermissionScreenState

eval BackPressed state = exit GoBack 

eval (ErrorModalActionController (ErrorModalController.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = do
  continueWithCmd state [do 
    conditionA <- isInternetAvailable unit
    if conditionA then do
      pure Reload
      else do
        pure NoAction
  ]

eval (LocationPermissionCallBackCustomer isLocationPermissionEnabled) state = do 
  let status = getLocationPermissionStatus unit
  let phonePermissionEnabled = isLocationEnabledWithoutEff unit
  let appPermissionEnabled = isLocationPermissionEnabledWithoutEff unit
  if not phonePermissionEnabled && appPermissionEnabled then do
    continue state {stage = LOCATION_DISABLED}
  else if isLocationPermissionEnabled && elem state.stage [LOCATION_DISABLED, LOCATION_DENIED] then updateAndExit state (LocationCallBack state)
    else do
     continue state {stage = (if status == "DENIED" then LOCATION_DENIED else state.stage)}
eval (InternetCallBackCustomer isInternetAvailable) state = do 
  if( isInternetAvailable == "true") then do
    updateAndExit state (InternetCallBack state)
    else continue state

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = continueWithCmd state [do
  push <- getPushFn Nothing "PermissionScreen"
  let _ = unsafePerformEffect $ do
        phonePermissionEnabled <- isLocationEnabled unit
        appPermissionEnabled <- isLocationPermissionEnabled unit
        if not phonePermissionEnabled && appPermissionEnabled then do
          askRequestedPermissionsWithCallback ["android.permission.ACCESS_FINE_LOCATION"] push LocationPermissionCallBackCustomer
        else do
          _ <- requestLocation unit
          pure unit
  pure $ UpdateState state
]

eval (UpdateState newState) state = continue newState

eval RequestLocation state = do 
  continue state

eval Reload state = updateAndExit state $ Refresh

eval (LocationBlockerPopUpAC PopUpModal.OnButton1Click) state = do
    _ <- pure $ launchAppSettings unit
    continue state
eval _ state = update state