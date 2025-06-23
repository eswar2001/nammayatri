{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.AddImagesModel.Controller where

import Prelude

import Components.PrimaryButton.Controller as PrimaryButton
import Styles.Colors (black700, black900, primaryButtonColor, white900) as Color
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM (Margin(..))
import Prelude (show, class Show, (<>))

instance showAction :: Show Action where
  show (OnClickDone var1) = "OnClickDone_" <> show var1
  show (OnClickCancel) = "OnClickCancel"
  show (OnClickDelete _) = "OnClickDelete"
  show (OnClickView _ _) = "OnClickView"
  show (AddImage) = "AddImage"
  show (BackPressed) = "BackPressed"
  show (NoAction) = "NoAction"

data Action = OnClickDone PrimaryButton.Action
            | OnClickCancel
            | OnClickDelete Int
            | OnClickView String String
            | AddImage
            | BackPressed
            | NoAction

type AddImagesModelState = {
  images :: Array Image,
  stateChanged :: Boolean,
  isLoading :: Boolean,
  imageMediaIds :: Array String,
  doneButtonText :: String,
  addedImagesText:: String,
  noImagesAddedText:: String,
  viewText:: String,
  deleteText:: String,
  addAnotherText:: String,
  addImageText:: String,
  noOfImages :: Int
}

type Image = {
  image :: String
, imageName :: String
}

config :: AddImagesModelState 
config = {
  images : [],
  stateChanged : false,
  isLoading : false,
  imageMediaIds : [],
  doneButtonText : "DONE",
  addedImagesText: "",
  noImagesAddedText: "",
  viewText: "",
  deleteText: "",
  addAnotherText: "",
  addImageText: "",
  noOfImages: 3
}

doneButtonConfig :: AddImagesModelState -> PrimaryButton.Config
doneButtonConfig state = let
    primaryButtonConfig' =  PrimaryButton.config
      { textConfig
      { text = state.doneButtonText
      , color = Color.primaryButtonColor
      }
      , cornerRadius = 8.0
      , background = Color.black900
      , height = V 48
      , alpha = 1.0 
      , isClickable = true 
      , enableLoader = state.isLoading
      , lottieConfig {
          height = V 15
        , width = V 75
      }
      , id = "add_images_model_done_button"
      , margin = Margin 0 0 0 0
      }
  in primaryButtonConfig'
