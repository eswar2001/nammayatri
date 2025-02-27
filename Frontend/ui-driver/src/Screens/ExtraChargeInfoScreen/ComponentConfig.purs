module Screens.ExtraChargeInfoScreen.ComponentConfig where

import Prelude
import Effect
import PrestoDOM

import Components.PrimaryButton as PrimaryButton

import Screens.ExtraChargeInfoScreen.Controller
import Screens.ExtraChargeInfoScreen.ScreenData


readMoreQA :: Array ({question :: String, answer :: String})
readMoreQA = [
    {
        question: "What is Extra-Charge Penalty ?",
        answer: "If driver asks extra from customer, then the overcharging score will increase."
    },
    {
        question: "What will happen if the penalty increases ?",
        answer: "Driver will get lesser rides if the score is high"
    },
    {
        question: "How can I reduce my penalty ?",
        answer: "Take rides and do not demand extra. Over time the score will reduce."
    },
    {
        question: "What if Driver does not stop asking extra ?",
        answer: "Driver will be suspended for a short duration. If driver continues asking extra, then driver will be blocked."
    }
]

gotItBtnConfig :: ExtraChargeInfoScreenState ->  PrimaryButton.Config
gotItBtnConfig state = PrimaryButton.config {
    textConfig {
        text = "Got it"
    }
,   margin = MarginTop 24
}


