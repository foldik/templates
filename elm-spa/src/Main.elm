module Main exposing (Model, Msg(..), init, main, pageView, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NewProjectForm
import NewUserForm
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
    | NewProjectFormPage NewProjectForm.Model
    | NewUserFormPage NewUserForm.Model


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

        Route.NewProject ->
            let
                ( pageModel, pageMsg ) =
                    NewProjectForm.init session
            in
            ( NewProjectFormPage pageModel, Cmd.map NewProjectFormMsg pageMsg )

        Route.NewUser ->
            let
                ( pageModel, pageMsg ) =
                    NewUserForm.init session
            in
            ( NewUserFormPage pageModel, Cmd.map NewUserFormMsg pageMsg )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Navigate Route.Route
    | NewProjectFormMsg NewProjectForm.Msg
    | NewUserFormMsg NewUserForm.Msg


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

        ( NewProjectFormMsg pageMsg, NewProjectFormPage pageModel ) ->
            let
                ( newPageModel, newPageMsg ) =
                    NewProjectForm.update pageMsg pageModel
            in
            ( { model | page = NewProjectFormPage newPageModel }, Cmd.map NewProjectFormMsg newPageMsg )

        ( NewUserFormMsg pageMsg, NewUserFormPage pageModel ) ->
            let
                ( newPageModel, newPageMsg ) =
                    NewUserForm.update pageMsg pageModel
            in
            ( { model | page = NewUserFormPage newPageModel }, Cmd.map NewUserFormMsg newPageMsg )

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
        [ a [ href (Route.toLink Route.NewProject) ]
            [ text (Route.toString Route.NewProject)
            ]
        , a [ href (Route.toLink Route.NewUser) ]
            [ text (Route.toString Route.NewUser) ]
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

        NewProjectFormPage pageModel ->
            NewProjectForm.view pageModel
                |> Html.map NewProjectFormMsg

        NewUserFormPage pageModel ->
            NewUserForm.view pageModel
                |> Html.map NewUserFormMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
