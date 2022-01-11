module PhotoGroove exposing (main)

import Browser
import Html exposing (h3, div, h1, img, text, Html, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Html exposing (button)

type ThumbnailSize = Small | Medium | Large

chosenSize: ThumbnailSize
chosenSize = Large

type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
   }

type alias Msg =
    { description : String, data : String }

photoListUrl: String
photoListUrl =
    "http://elm-in-action.com/"

initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }

toList : List a -> Array a
toList input = Array.fromList input

photoArray: Array Photo
photoArray = toList initialModel.photos

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick { description = "ClickedSupriseMe", data = "" } ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img [ class "large"
            , src (photoListUrl ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium  ->
            "mid"

        Large ->
            "large"


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size" ] []
        , text (sizeToString size)
        ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (photoListUrl ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []

update : Msg -> Model -> Model
update msg model =
    case msg.description of
    "ClickedPhoto" ->
        { model | selectedUrl = msg.data }

    "ClickedSupriseMe" ->
        { model | selectedUrl = "2.jpeg" }

    _ ->
        model

main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
