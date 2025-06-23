{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ReferralScreen.Controller where

import Components.BottomNavBar as BottomNavBar
import Components.GenericHeader as GenericHeader
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PrimaryEditText.Controllers as PrimaryEditText
import Data.Array (last, (!!), init, replicate, filter, sortWith, any)
import Data.Array (length) as DA
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe)
import Data.String (length)
import Debug (spy)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, getCurrentUTC, getPastMonths)
import Engineering.Helpers.LogEvent (logEvent)
import Helpers.Utils (setRefreshing, getPastDays, getPastWeeks, convertUTCtoISC, generateQR, incrementValueOfLocalStoreKey, contactSupportNumber)
import JBridge (hideKeyboardOnNavigation, toast, showDialer, firebaseLogEvent, scrollToEnd, cleverTapCustomEvent, metaLogEvent, shareImageMessage)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (bind, class Show, pure, unit, ($), discard, (>=), (<=), (==), (&&), not, (+), show, void, (<>), when, map, (-), (>), (/=))
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.ReferralScreen.ScreenData as RSD
import Screens.Types (ReferralScreenState, ReferralType(..), LeaderBoardType(..), DateSelector(..), RankCardData)
import Services.API (LeaderBoardRes(..), DriversInfo(..), GetPerformanceRes(..), GenerateReferralCodeRes(..))
import Storage (KeyStore(..), getValueToLocalNativeStore, setValueToLocalNativeStore)
import Effect.Aff (launchAff_)
import Common.Types.App
import Effect.Uncurried(runEffectFn4)
import Storage(KeyStore(..), getValueToLocalStore)
import ConfigProvider
import Timers (clearTimerWithId)
import Data.String as DS

instance showAction :: Show Action where
  show (BottomNavBarAction var1) = "BottomNavBarAction_" <> show var1
  show (GenericHeaderActionController var1) = "GenericHeaderActionController_" <> show var1
  show (PrimaryEditTextAction1 var1) = "PrimaryEditTextAction1_" <> show var1
  show (PrimaryEditTextAction2 var1) = "PrimaryEditTextAction2_" <> show var1
  show (PrimaryButtonActionController var1) = "PrimaryButtonActionController_" <> show var1
  show (PasswordModalAction var1) = "PasswordModalAction_" <> show var1
  show (SuccessScreenExpireCountDwon _ _ _) = "SuccessScreenExpireCountDwon"
  show (ContactSupportAction var1) = "ContactSupportAction_" <> show var1
  show (GoToAlertScreen) = "GoToAlertScreen"
  show (EnableReferralFlow) = "EnableReferralFlow"
  show (BackPressed) = "BackPressed"
  show (RefreshScreen) = "RefreshScreen"
  show (EnableReferralFlowNoAction) = "EnableReferralFlowNoAction"
  show (SuccessScreenRenderAction) = "SuccessScreenRenderAction"
  show (ChangeLeaderBoardtab _) = "ChangeLeaderBoardtab"
  show (DateSelectorAction) = "DateSelectorAction"
  show (ChangeDate _) = "ChangeDate"
  show (UpdateLeaderBoard _) = "UpdateLeaderBoard"
  show (AfterRender) = "AfterRender"
  show (UpdateLeaderBoardFailed) = "UpdateLeaderBoardFailed"
  show (ReferralQrRendered _) = "ReferralQrRendered"
  show (NoAction) = "NoAction"
  show (ShareOptions) = "ShareOptions"
  show (UpdateDriverPerformance _) = "UpdateDriverPerformance"
  show (UpdateReferralCode _) = "UpdateReferralCode"
  show (UpdateDriverPerformanceFailed) = "UpdateDriverPerformanceFailed"
  show (UpdateReferralCodeFailed) = "UpdateReferralCodeFailed"

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REFERRAL_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REFERRAL_SCREEN)
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    RefreshScreen -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "refresh"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen REFERRAL_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_button" "link_referral_code"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "primary_button" "no_action"
    PrimaryEditTextAction1 act -> case act of
      PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_text_changed"
      PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "referral_code_text_field_click"
    PrimaryEditTextAction2 act -> case act of
      PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "confirm_referral_code_text_changed"
      PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "primary_edit_text" "confirm_referral_code_text_field_click"
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    PasswordModalAction act -> case act of
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "confirm_password"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      PopUpModal.ETextController act -> case act of
        PrimaryEditTextController.TextChanged valId newVal -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_changed" "popup_modal_edit_password"
        PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_focus_changed" "popup_modal_edit_password"
        PrimaryEditTextController.TextImageClicked -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "text_image_onclick"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "close_icon"
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "no_action"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "countdown_updated"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "password_popup_modal_action" "no_action"
      PopUpModal.YoutubeVideoStatus _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "youtube_video_status"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "secondary_text_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "popup_dismissed"
      _ -> pure unit
    SuccessScreenExpireCountDwon seconds status timerId -> do
      if status == "EXPIRED" then trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "countdown_expired"
        else trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "countdown_updated"
    ContactSupportAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "cancel"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "call_support"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "close_icon"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "countdown_updated"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "contact_support_popup_modal_action" "no_action"
      PopUpModal.YoutubeVideoStatus _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "youtube_video_status"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "secondary_text_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "popup_modal_action" "popup_dismissed"
      _ -> pure unit
    GoToAlertScreen -> do
      trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "for_updates_see_alerts"
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    EnableReferralFlow -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "enable_referral_flow"
    EnableReferralFlowNoAction -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "enable_referral_flow_no_action"
    SuccessScreenRenderAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "your_referral_code_is_linked"
    ChangeLeaderBoardtab _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "change_leader_board_tab"
    DateSelectorAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "date_selector_clicked"
    ChangeDate date ->
      case date of
        DaySelector _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "day_changed"
        WeekSelector _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "week_changed"
        MonthSelector _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "month_changed"
    UpdateLeaderBoard _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_leaderBoard"
    UpdateLeaderBoardFailed -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_leaderBoard_failed"
    ReferralQrRendered _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "referral_qr_rendered"
    NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "no_action"
    ShareOptions -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "share_options"
    UpdateDriverPerformance _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_driver_performance"
    UpdateReferralCode _ -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_referral_code"
    UpdateDriverPerformanceFailed -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_driver_performance_failed"
    UpdateReferralCodeFailed -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "in_screen" "update_referral_code_failed"

data Action = BottomNavBarAction BottomNavBar.Action
            | GenericHeaderActionController GenericHeader.Action
            | PrimaryEditTextAction1 PrimaryEditText.Action
            | PrimaryEditTextAction2 PrimaryEditText.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | PasswordModalAction PopUpModal.Action
            | SuccessScreenExpireCountDwon Int String String
            | ContactSupportAction PopUpModal.Action
            | GoToAlertScreen
            | EnableReferralFlow
            | BackPressed
            | RefreshScreen
            | EnableReferralFlowNoAction
            | SuccessScreenRenderAction
            | ChangeLeaderBoardtab LeaderBoardType
            | DateSelectorAction
            | ChangeDate DateSelector
            | UpdateLeaderBoard LeaderBoardRes
            | AfterRender
            | UpdateLeaderBoardFailed
            | ReferralQrRendered String
            | NoAction
            | ShareOptions
            | UpdateDriverPerformance GetPerformanceRes
            | UpdateReferralCode GenerateReferralCodeRes
            | UpdateDriverPerformanceFailed
            | UpdateReferralCodeFailed


data ScreenOutput = GoToHomeScreen ReferralScreenState
                  | GoBack
                  | GoToRidesScreen ReferralScreenState
                  | GoToProfileScreen ReferralScreenState
                  | GoToNotifications ReferralScreenState
                  | LinkReferralApi ReferralScreenState
                  | Refresh ReferralScreenState
                  | SubscriptionScreen ReferralScreenState
                  | EarningsScreen

eval :: Action -> ReferralScreenState -> Eval Action ScreenOutput ReferralScreenState

eval (UpdateLeaderBoard (LeaderBoardRes leaderBoardRes)) state = do
  _ <- pure $ firebaseLogEvent "ny_driver_leaderboard"
  let dataLength = DA.length leaderBoardRes.driverList
      rankersData = sortWith (_.rank) (transformLeaderBoardList (filter (\(DriversInfo info) -> info.rank <= 10 && info.totalRides > 0 && info.rank > 0) leaderBoardRes.driverList) state.data.config.leaderBoard.isMaskedName) <> (replicate (10 - dataLength) RSD.dummyRankData)
      currentDriverData = case (filter (\(DriversInfo info) -> info.isCurrentDriver && info.rank > 0 && info.totalRides > 0) leaderBoardRes.driverList) !! 0 of
                            Just driverData -> transformLeaderBoard driverData state.data.config.leaderBoard.isMaskedName
                            Nothing         -> RSD.dummyRankData
      lastUpdatedAt = convertUTCtoISC (fromMaybe (getCurrentUTC "") leaderBoardRes.lastUpdatedAt) "h:mm A"
  let newState = state{ props { rankersData = rankersData, currentDriverData = currentDriverData, showShimmer = false, noData = not (dataLength > 0), lastUpdatedAt = lastUpdatedAt } }
  _ <- pure $ setRefreshing (getNewIDWithTag "ReferralRefreshView") false
  if (any DS.null [state.props.selectedDay.utcDate, state.props.selectedWeek.utcStartDate, state.props.selectedWeek.utcEndDate, state.props.selectedMonth.utcDate]) then do
    let pastDates = getPastDays 7
        pastWeeks = getPastWeeks 4
        pastMonths = getPastMonths 3
        selectedDay = case last pastDates of
                        Just date -> date
                        Nothing -> state.props.selectedDay
        selectedWeek = case last pastWeeks of
                        Just week -> week
                        Nothing -> state.props.selectedWeek
        selectedMonth = fromMaybe state.props.selectedMonth $ last pastMonths
    continue newState{ props{ days = pastDates, weeks = pastWeeks, months = pastMonths, selectedDay = selectedDay, selectedWeek = selectedWeek, selectedMonth = selectedMonth} }
  else continue newState

eval UpdateLeaderBoardFailed state = do 
  _ <- pure $ setRefreshing (getNewIDWithTag "ReferralRefreshView") false
  continue state{ props{ showShimmer = false, noData = true } }

eval (UpdateDriverPerformance (GetPerformanceRes performanceRes)) state = continue state {data {driverInfo {referralCode = Just (getValueToLocalStore REFERRAL_CODE)},driverPerformance{referrals{totalActivatedCustomers = performanceRes.referrals.totalActivatedCustomers,totalReferredCustomers = performanceRes.referrals.totalReferredCustomers, totalReferredDrivers = fromMaybe 0 performanceRes.referrals.totalReferredDrivers}}} , props{showShimmer =  if (getValueToLocalStore REFERRAL_CODE) /= "__failed" then false else state.props.showShimmer}}

eval (UpdateDriverPerformanceFailed) state = continue state {props{showShimmer= false}}

eval (UpdateReferralCode (GenerateReferralCodeRes referralCode)) state = continue state{data{driverInfo {referralCode = Just referralCode.referralCode}}, props{showShimmer = false}}

eval (UpdateReferralCodeFailed) state = continue state {props{showShimmer = false}}

eval (ChangeDate (DaySelector item)) state = do
  if state.props.selectedDay == item then
    continue state
  else do
    let newState = state { props { selectedDay = item, showShimmer = true } }
    updateAndExit newState $ Refresh newState

eval (ChangeDate (WeekSelector item)) state =
  if state.props.selectedWeek == item then
    continue state
  else do
    let newState = state { props { selectedWeek = item, showShimmer = true } }
    updateAndExit newState $ Refresh newState

eval (ChangeDate (MonthSelector item)) state = 
  if state.props.selectedMonth == item then 
    continue state
  else do
    let newState = state { props { selectedMonth = item, showShimmer = true } }
    updateAndExit newState $ Refresh newState

eval DateSelectorAction state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "DateSelector") false
  continue state { props { showDateSelector = not state.props.showDateSelector } }

eval (ChangeLeaderBoardtab tab) state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "DateSelector") false
  let newState = state { props { leaderBoardType = tab, showShimmer = true } }
  updateAndExit newState $ Refresh newState

eval BackPressed state = exit $ GoToHomeScreen state

eval RefreshScreen state = exit $ Refresh state

eval EnableReferralFlow state = do
  if (state.props.enableReferralFlowCount >= 5 ) then do
    continue state {props {stage = ReferralFlow}}
    else do
      continue state {props {enableReferralFlowCount = state.props.enableReferralFlowCount + 1}}

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = continue state { props = state.props { passwordPopUpVisible = not state.props.passwordPopUpVisible }}

eval GoToAlertScreen state = do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      exit $ GoToNotifications state

eval (PrimaryEditTextAction1 (PrimaryEditText.TextChanged valId newVal)) state = do
  _ <- if length newVal >= 6 then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
  continue state {  props = state.props { primarybtnActive = ((length newVal) == 6 && (newVal == state.data.confirmReferralCode))}
                    , data = state.data { referralCode = if length newVal <= 6 then newVal else state.data.referralCode }}

eval (PrimaryEditTextAction2 (PrimaryEditText.TextChanged valId newVal)) state = do
  _ <- if length newVal >= 6 then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
  continue state {  props = state.props { primarybtnActive = ((length newVal) == 6 && (newVal == state.data.referralCode))}
                   , data = state.data { confirmReferralCode = if length newVal <= 6 then newVal else state.data.confirmReferralCode }}


eval (PasswordModalAction (PopUpModal.OnImageClick)) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    continue state { data = state.data{ password = "" }, props = state.props { passwordPopUpVisible = not state.props.passwordPopUpVisible , confirmBtnActive = false }}


eval (PasswordModalAction (PopUpModal.OnButton2Click)) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    exit $ LinkReferralApi state

eval ShareOptions state = do
  _ <- pure $ shareImageMessage (getMessage (getMerchant FunctionCall) state.data.driverInfo.referralCode) (shareImageMessageConfig state)
  continue state

eval (PasswordModalAction (PopUpModal.ETextController (PrimaryEditTextController.TextChanged valId newVal))) state = do
  _ <- if length newVal >= 5 then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
  continue state{ data{ password = newVal } , props { confirmBtnActive = (length newVal == 5)}}

eval (ContactSupportAction (PopUpModal.OnButton1Click)) state = continue state { props = state.props { callSupportPopUpVisible = not state.props.callSupportPopUpVisible  }}
eval (ContactSupportAction (PopUpModal.OnButton2Click)) state = do
    void $ pure $ unsafePerformEffect $ contactSupportNumber ""-- TODO: FIX_DIALER -- unsafePerformEffect is a temporary fix, need to update this.
    continue state { props = state.props { callSupportPopUpVisible = not state.props.callSupportPopUpVisible  }}

eval (SuccessScreenExpireCountDwon seconds status timerId) state = if status == "EXPIRED" then do
  _ <- pure $ clearTimerWithId timerId
  continue state{props {stage = QRScreen}} else continue state

eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  pure $ hideKeyboardOnNavigation true
  case item of
    "Home" -> exit $ GoToHomeScreen state
    "Rides" -> exit $ GoToRidesScreen state
    "Profile" -> exit $ GoToProfileScreen state
    "Earnings" -> exit $ EarningsScreen
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_alert_click"
      exit $ GoToNotifications state
    "Join" -> do
      let driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      void $ pure $ incrementValueOfLocalStoreKey TIMES_OPENED_NEW_SUBSCRIPTION
      _ <- pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      _ <- pure $ metaLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      let _ = unsafePerformEffect $ firebaseLogEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (ReferralQrRendered id) state = 
  continueWithCmd state [ do
    runEffectFn4 generateQR state.data.config.referral.link id 200 0
    pure $ NoAction
  ]
eval _ state = update state


transformLeaderBoardList :: (Array DriversInfo) -> Boolean -> Array RankCardData
transformLeaderBoardList driversList isMaskedName = map (\x -> transformLeaderBoard x isMaskedName) driversList

transformLeaderBoard :: DriversInfo -> Boolean -> RankCardData
transformLeaderBoard (DriversInfo driversInfo) isMaskedName = 
  {
      goodName : if isMaskedName then "*******" else driversInfo.name
    , profileUrl : Nothing
    , rank : driversInfo.rank
    , rides : driversInfo.totalRides
    , gender : driversInfo.gender
  }

getReferralStage :: ReferralScreenState -> ReferralType
getReferralStage state =
  case state.data.config.referral.type of
    "LeaderBoard" -> LeaderBoard
    "QRScreen" -> QRScreen
    _ -> LeaderBoard

getMessage :: Merchant -> Maybe String -> String
getMessage merchant referralCode=
  case merchant of
    NAMMAYATRI -> "👋 Hey,\n\nMy Namma Yatri Referral Code is " <> ( fromMaybe "" referralCode)  <> ".\n\nScan the QR code and download Namma Yatri app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
    YATRI -> "👋 Hey,\n\nMy Yatri Referral Code is " <> ( fromMaybe "" referralCode)  <> ".\n\nScan the QR code and download Yatri app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
    YATRISATHI -> "👋 Hey,\n\nMy Yatri Sathi Referral Code is " <> ( fromMaybe "" referralCode)  <> ".\n\nScan the QR code and download Yatri Sathi app. You can help me out by entering my referral code on the Home screen.\n\nThanks!"
    _ -> ""

shareImageMessageConfig :: ReferralScreenState -> ShareImageConfig
shareImageMessageConfig state = {
  code : fromMaybe "" state.data.driverInfo.referralCode,
  viewId : getNewIDWithTag "ReferralQRScreen",
  logoId : getNewIDWithTag "ReferralScreenLogo",
  isReferral : true
  }
