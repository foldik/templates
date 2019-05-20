module Page.Resource exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Page.NotFound as NotFoundPage
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
    ( Model session (Loading id), ResourceApi.getResource id GotResourceResponse )



-- UPDATE


type Msg
    = NoOp ()
    | GotResourceResponse (Result Http.Error ResourceApi.Resource)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        GotResourceResponse result ->
            case result of
                Ok resource ->
                    ( { model | resource = Loaded resource }, Cmd.none )

                Err _ ->
                    ( { model | resource = NotFound }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.resource of
        Loading id ->
            div [] [ text "Loading" ]

        NotFound ->
            Html.map NoOp NotFoundPage.view

        Loaded resource ->
            h1 [ class "title is-1" ]
                [ text resource.name ]
