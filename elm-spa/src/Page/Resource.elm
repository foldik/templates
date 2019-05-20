module Page.Resource exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Url



-- MODEL


type alias Model =
    { session : Session.Session
    , resource : Loadable
    }


type Loadable
    = Loading Int
    | Loaded ResourceApi.Resource
    | NotFound


init : Session.Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( Model session (Loading id), Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    h1 [ class "title is-1" ]
        [ text "One resource" ]
