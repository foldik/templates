module Form exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session
import Time
import Url
import User



-- MODEL


type alias Model =
    { session : Session.Session
    , serviceName : String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session "", Cmd.none )



-- UPDATE


type Msg
    = UpdateServiceName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateServiceName value ->
            ( { model | serviceName = value }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ modelView model
        , Html.form []
            [ fieldset []
                [ legend []
                    [ text "New Service" ]
                , div []
                    [ label []
                        [ text "Service name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Service name", value model.serviceName, onInput UpdateServiceName ]
                        []
                    ]
                ]
            ]
        ]


modelView : Model -> Html Msg
modelView model =
    div []
        [ h1 []
            [ text "Model" ]
        , p []
            [ text ("serviceName: " ++ model.serviceName) ]
        ]
