module Main exposing (Model, Msg(..), init, main, pageView, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session
import Time
import Url
import User


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { session : Session.Session
    , page : Page
    }


type Page
    = NotFound
    | Home
    | FormPage Form.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            { user = Nothing, url = url, key = key, timeZone = Time.utc }

        ( page, command ) =
            routeToPage session (Route.router url)
    in
    ( { session = session, page = page }, command )


routeToPage : Session.Session -> Route.Route -> ( Page, Cmd Msg )
routeToPage session route =
    case route of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.Home ->
            ( Home, Cmd.none )

        Route.Form ->
            let
                ( pageModel, pageMsg ) =
                    Form.init session
            in
            ( FormPage pageModel, Cmd.map FormMsg pageMsg )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Navigate Route.Route
    | FormMsg Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( Navigate route, _ ) ->
            ( model, Nav.pushUrl model.session.key (Route.toLink route) )

        ( UrlChanged url, _ ) ->
            let
                oldSession =
                    model.session

                session =
                    { oldSession | url = url }

                ( page, command ) =
                    routeToPage session (Route.router url)
            in
            ( { model | session = session, page = page }, command )

        ( FormMsg pageMsg, FormPage pageModel ) ->
            let
                ( newPageModel, newPageMsg ) =
                    Form.update pageMsg pageModel
            in
            ( { model | page = FormPage newPageModel }, Cmd.map FormMsg newPageMsg )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello"
    , body =
        [ navBar model
        , pageView model
        ]
    }


navBar : Model -> Html Msg
navBar model =
    nav []
        [ select []
            [ option [ value "form", onClick (Navigate Route.Form) ]
                [ text "Form" ]
            ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        NotFound ->
            h1 []
                [ text "Not found" ]

        Home ->
            h1 []
                [ text "Home" ]

        FormPage pageModel ->
            Form.view pageModel
                |> Html.map FormMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
