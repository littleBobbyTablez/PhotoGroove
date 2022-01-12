module PhotoGroove exposing (main)

import Browser
import Html exposing (h3, div, h1, img, text, Html, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Html exposing (button)
import Random
import Http

type ThumbnailSize = Small | Medium | Large

type alias Photo =
    { url : String }

type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
   }

type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSupriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

photoListUrl: String
photoListUrl =
    "http://elm-in-action.com/"

initialModel : Model
initialModel =
    { status  = Loading
    , chosenSize = Large
    }

initialCmd : Cmd Msg
initialCmd =
    Http.get
    { url = photoListUrl ++ "photos/list"
    , expect = Http.expectString GotPhotos
    }

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium  ->
            "med"

        Large ->
            "large"



-- view

view : Model -> Html Msg
view model =
    div [ class "content" ]
        <|
            case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded  photos selectedUrl chosenSize =
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSupriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map (viewThumbnail selectedUrl) photos)
        , img [ class "large"
            , src (photoListUrl ++ "large/" ++ selectedUrl)
            ]
            []
        ]


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (photoListUrl ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


-- update

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
    ClickedPhoto url ->
        ( { model | status = selectUrl url model.status }, Cmd.none )

    ClickedSupriseMe ->
        case model.status of

            Loaded (firstPhoto :: otherPhotos) _ ->
                Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto
                    |> Tuple.pair model

            Loaded [] _ ->
                (model, Cmd.none)

            Loading ->
                (model, Cmd.none)

            Errored _ ->
                (model, Cmd.none)


    ClickedSize size ->
        ( { model | chosenSize = size }, Cmd.none )

    GotRandomPhoto photo ->
        ( { model | status  = selectUrl photo.url model.status }, Cmd.none )


    GotPhotos (Ok responseStr) ->
        case String.split "," responseStr of
            (firstUrl :: _) as urls ->
                let
                    photos =
                        List.map Photo urls
                in
                ( { model | status = Loaded photos firstUrl }, Cmd.none )

            [] ->
                ( { model | status = Errored "0 photos found"}, Cmd.none )

    GotPhotos (Err _) ->
        (model, Cmd.none)

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, initialCmd  )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
