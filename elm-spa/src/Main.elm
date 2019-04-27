module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Api
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Role
import Route
import Session
import Url
import User


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



--MODEL


type alias Model =
    { session : Session.Session
    }


type Page
    = Loading
    | NotFound
    | Home


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            Session.guest key url
    in
    ( Model session
    , Api.loadSession GotSession
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotSession (Result Http.Error User.User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession result ->
            case result of
                Ok user ->
                    ( { model | session = Session.toSignedIn model.session user }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm SPA"
    , body =
        [ div []
            [ div []
                [ text "Hello" ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
