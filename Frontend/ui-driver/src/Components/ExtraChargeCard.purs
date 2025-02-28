module Components.ExtraChargeCard where

import PrestoDOM
import Prelude
import Font.Style as FontStyle
import Styles.Colors as Colors
import Engineering.Helpers.Commons
import Components.PrimaryButton as PrimaryButton
import Effect
import Data.Maybe
import Common.Types.App
import Helpers.Utils
import Animation
import PrestoDOM.Animation as PrestoAnim

data Action = LearnMoreExtraChargeBtnAC PrimaryButton.Action

type ExtraChargeCardType = {
  showGotItBtnView :: Visibility
, badge :: BadgeConfig
}

type BadgeConfig = {
  pillTitle :: String
, title :: String
, subTitle :: Maybe String
, description :: Maybe String
, image :: String

, pillBackground :: String
, imageBackground :: String

, gaugeAngle :: Maybe Number
}

getBadgeConfig :: String -> BadgeConfig
getBadgeConfig stage = case stage of
  "" -> {
    pillTitle: "Zero",
    title : "Nyayamana Rate'u Karan",
    subTitle : Nothing,
    description : Nothing,
    image : fetchImage FF_COMMON_ASSET "ny_ic_nyamana_rateu_karan",

    pillBackground : Colors.green900,
    imageBackground : "#53BB6F14",

    gaugeAngle : Just 0.0
  }
  _ ->  {
    pillTitle: "Low",
    title : "6 out of 20 rides",
    subTitle : Just "Extra-charged",
    description : Nothing,
    image : fetchImage FF_COMMON_ASSET "ny_ic_extra_charge_gauge",

    pillBackground : Colors.yellow900,
    imageBackground : "#53BB6F14",

    gaugeAngle : Just 0.0
  }
  _ ->  {
    pillTitle: "High",
    title :  "6 out of 20 rides",
    subTitle : Just "Extra-charged",
    description : Nothing,
    image : fetchImage FF_COMMON_ASSET "ny_ic_extra_charge_gauge",

    pillBackground : Colors.orange900,
    imageBackground : "#53BB6F14",

    gaugeAngle : Just 0.0
  }
  _ -> {
    pillTitle: "Suspended",
    title :  "6 out of 20 rides",
    subTitle : Just "Extra-charged",
    description : Nothing,
    image : fetchImage FF_COMMON_ASSET "ny_ic_extra_charge_gauge",

    pillBackground : Colors.red900,
    imageBackground : "#53BB6F14",

    gaugeAngle : Just 0.0
  }
  _ -> {
    pillTitle: "Blocked",
    title :  "6 out of 20 rides",
    subTitle : Just "Extra-charged",
    description : Nothing,
    image : fetchImage FF_COMMON_ASSET "ny_ic_blocked",

    pillBackground : Colors.red900,
    imageBackground : "#53BB6F14",

    gaugeAngle : Nothing
  }


view :: forall w. (Action -> Effect Unit) -> ExtraChargeCardType -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout[
    width MATCH_PARENT,
    height WRAP_CONTENT,
    orientation VERTICAL
  ][
    textView $ [
      text "Extra-Charge Penalty",
      width MATCH_PARENT,
      height WRAP_CONTENT,
      color Colors.black
    , margin $ MarginBottom 12
    ] <> (FontStyle.subHeading2 TypoGraphy),
     linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , background Colors.aliceBlueLight
    , orientation VERTICAL
    , cornerRadius 16.0
    ] $ [
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 16
      ][
        linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        ][
          linearLayout[
            width $ V $ ((screenWidth unit) - 64 ) / 2
          , height WRAP_CONTENT
          , orientation VERTICAL
          , gravity CENTER
          ][
            pillView
          , titleView
          , subTitleView
          , descriptionView
          ]
        , frameLayout[
              width $ V $ ((screenWidth unit) - 64) / 2
            , height $ V 90
            ][
              linearLayout[
                  cornerRadius 16.0
                , background config.badge.imageBackground
                , padding $ Padding 16 4 16 4
                  , gravity CENTER
              ][
                imageView[
                  height $ V 90
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_extra_charge_gauge"
                ]
              ]
              , gaugeView
            ]
        ]
      ]
    ] <> if config.showGotItBtnView == VISIBLE then [learnMoreBtn] else []
  ]

  where
    pillView =
      linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 16 4 16 4
      , background config.badge.pillBackground
      , cornerRadius 20.0
      , gravity CENTER
      , margin $ MarginBottom 8
      ][
        textView $ [
          text config.badge.pillTitle
        , color Colors.white900
        , padding $ PaddingBottom 4
        ] <> (FontStyle.body30 TypoGraphy)
      ]

    titleView =
      textView $ [
        text config.badge.title
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , color Colors.black800
      , gravity CENTER
      , margin $ MarginBottom 4
      ] <> (FontStyle.body4 TypoGraphy)

    subTitleView =
      case config.badge.subTitle of
        Just subTitle ->
          textView $ [
            text subTitle
          , color Colors.black800
          , gravity CENTER
          ] <> (FontStyle.body3 TypoGraphy)
        Nothing -> linearLayout [width $ V 0, height $ V 0][]

    descriptionView =
      case config.badge.description of
        Just description ->
          textView $ [
            text description
          , color Colors.black800
          , gravity CENTER
          ] <> (FontStyle.body3 TypoGraphy)
        Nothing -> linearLayout [width $ V 0, height $ V 0][]

    gaugeView =
      case config.badge.gaugeAngle of
        Just angle ->
          linearLayout
            [
              gravity CENTER
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , layoutGravity "bottom"
            , alignParentBottom "true,-1"

            ] [
                PrestoAnim.animationSet [ PrestoAnim.Animation [PrestoAnim.fromRotation 270, PrestoAnim.toRotation 360, PrestoAnim.interpolator interpolator, PrestoAnim.duration 2000] true ]$
                  imageView[
              height $ V 120
            , width $ V 60
             , margin $ MarginTop 35
            , rotation $ 270.0
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_ec_gauge"
            ]
            ]
        Nothing -> linearLayout [width $ V 0, height $ V 0][]


    learnMoreBtn  = PrimaryButton.view (push <<< LearnMoreExtraChargeBtnAC) learnMoreExtraChargeBtnConfig


learnMoreExtraChargeBtnConfig :: PrimaryButton.Config
learnMoreExtraChargeBtnConfig = PrimaryButton.config {
  textConfig {
    text = "Learn More"
  , color = Colors.blue800
  }
  , background = "#E2EFFF"
  , margin = Margin 16 16 16 16
}
