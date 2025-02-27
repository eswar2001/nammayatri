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
import PrestoDOM.Properties
import PrestoDOM.Elements.Elements
import Helpers.Utils
import Screens.ExtraChargeInfoScreen.ComponentConfig
import Components.PrimaryButton as PrimaryButton
import Common.Types.App as CTA
import Data.Function.Uncurried
import Data.Array as DA
import JBridge
import PrestoDOM.Core
import Debug

screen :: ExtraChargeInfoScreenState -> ScopedScreen Action ExtraChargeInfoScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , parent : Just "ExtraChargeInfoScreen"
  , name : "ExtraChargeInfoScreen"
  , globalEvents : []
  , eval : (\action state -> do
      let _ = spy "ExtraChargeInfoScreen state: " state
      let _ = spy "ExtraChargeInfoScreen action: " action
      eval action state)
  }


view ::forall w .  (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
view push state =
  let
    _  = spy "ExtraChargeInfoScreen" "inside View"
  in
    linearLayout[
        height MATCH_PARENT
      , width MATCH_PARENT
      , background "#502C2F3A"
      , orientation VERTICAL
      , gravity BOTTOM
      , onBackPressed push $ const OnBackPressed
    ][
      coordinatorLayout[
        height WRAP_CONTENT
      , width MATCH_PARENT
      ][
        bottomSheetLayout[
          width MATCH_PARENT
        , peakHeight $ (screenHeight unit) - 200
        , sheetState HIDDEN
        , halfExpandedRatio 0.9
        , hideable true
        , background Color.white900
        , cornerRadii $ Corners 24.0 true true false false
        , padding $ Padding 16 8 16 16
        , orientation VERTICAL
        , onStateChanged push $ BottomSheetStageChanged
        , onSlide push $ BottomSheetSlide
        ][linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  ][
          handlerView
        , bodyView push state
        , gotItBtnView push state
        , needHelpView push state
                  ]
        ]
      ]
    ]

handlerView :: forall w. PrestoDOM (Effect Unit) w
handlerView =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginBottom 12
  ][
    linearLayout[
      width $ V 24
    , height $ V 4
    , cornerRadius 2.0
    , background Color.argent
    ][]
  ]

bodyView :: forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
bodyView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][
    textView $ [
      text "Your Extra-Charge Penalty"
    , color Color.black800
    , width MATCH_PARENT
    , gravity CENTER
    , margin $ MarginBottom 12
    ] <> (FontStyle.h2 TypoGraphy)
  , questionAndAnswersView push state
  ]

questionAndAnswersView ::  forall w . (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
questionAndAnswersView push state =
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ] $ DA.mapWithIndex (\index item -> questionAndAnswerView item.question item.answerView index push state) mainQA

questionAndAnswerView :: forall w . String
  -> ((Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w)
  -> Int
  -> (Action  -> Effect Unit)
  -> ExtraChargeInfoScreenState
  -> PrestoDOM (Effect Unit) w
questionAndAnswerView questionStr answerView index push state =
  let optionOpened = fromMaybe false (state.optionOpened DA.!! index)
  in
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.aliceBlueLight
    , padding $ Padding 16 16 16 16
    , cornerRadius 16.0
    , orientation VERTICAL
    , margin $ MarginTop (if index == 0 then 0 else 24)
    ]$ [
      questionView questionStr index optionOpened push state
    ] <> (if optionOpened then [ answerView push state] else [])

questionView ::  forall w . String -> Int -> Boolean ->  (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
questionView questionStr ind optionOpened push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , onClick push $ const $ OnQuestionClick ind
  ][
    textView $ [
        text questionStr
      , color Color.black800
      ] <> (FontStyle.h2 TypoGraphy)
  , linearLayout[weight 1.0, height WRAP_CONTENT][]
  , imageView[
      imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down_light"
    , height $ V 24
    , width $ V 24
    , rotation if optionOpened then 180.0 else 0.0
    ]
  ]

answer1View :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
answer1View push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 12
  ][
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity CENTER
      , id (getNewIDWithTag "extraChargeVideoView")
      , afterRender
          ( \action -> do
              let
                id = (getNewIDWithTag "extraChargeVideoView")
                url = "https://youtu.be/2MLe3lLmjyg?si=PaLEg6_wQ6TmEej8"
              void $ pure $ runFn5 setYoutubePlayer (getYoutubeData{videoType = "VIDEO",videoId = getVideoID url}) id (show CTA.PLAY) push YoutubeVideoStatus
              push $ PauseVideo
          )
          (const PauseVideo)
      ][]
  ]



answer2View :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
answer2View push state =
  linearLayout [
    width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop 12
  ] (DA.mapWithIndex (\index item -> qAView index item ) readMoreQA)

  where
    qAView ind qa =
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop if ind /= 0 then  8 else  0
      ][
        textView $ [
          text qa.question
        , color Color.black700
        ] <> (FontStyle.body4 TypoGraphy)
      , textView $ [
          text qa.answer
        , color Color.black700
        ] <> (FontStyle.body2 TypoGraphy)
      ]

mainQA :: forall w. Array ({question:: String, answerView:: (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w})
mainQA = [
    {
        question : "What does this mean?"
    ,   answerView : answer1View
    },
    {
        question : "Read More",
        answerView : answer2View
    }
]


gotItBtnView :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
gotItBtnView push state = PrimaryButton.view (push <<< GotItBtn) (gotItBtnConfig state)

needHelpView :: forall w. (Action  -> Effect Unit) -> ExtraChargeInfoScreenState -> PrestoDOM (Effect Unit) w
needHelpView push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginTop 18
  , gravity CENTER
  ][
    textView $ [
      text "Need Help?"
    , color Color.black650
    ] <> (FontStyle.body2 TypoGraphy)
  , imageView[
      imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_call"
    , width $ V 12
    , height $ V 12
    , margin $ MarginHorizontal 2 2
    ]
  , textView $ [
      text "Call Support"
    , color Color.blue800
    , onClick push $ const OnCallSupportAC
    ] <> (FontStyle.body2 TypoGraphy)
  ]
