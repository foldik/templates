module Page.Home exposing (Model, Msg, init, update, view)

import Command
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
    { value : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Command.send (InitPage 2) )



-- UPDATE


type Msg
    = InitPage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitPage value ->
            ( { model | value = Just value }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title is-1" ]
            [ text
                (case model.value of
                    Just id ->
                        String.fromInt id

                    Nothing ->
                        "Nothing sorry"
                )
            ]
        ]
