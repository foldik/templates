module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Command
import Dummy
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Role as Role
import Model.Session as Session
import Model.User as User
import Navbar
import Page.Home as HomePage
import Page.Loading as LoadingPage
import Page.Login as LoginPage
import Page.Logout as LogoutPage
import Page.NotFound as NotFoundPage
import Page.Preferences as PreferencesPage
import Page.Resource as ResourcePage
import Page.Resources as ResourcesPage
import Route
import Task
import Time
import Url


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
    , navbar : Navbar.Model
    , page : Page
    }


type Page
    = Loading
    | NotFound
    | Home HomePage.Model
    | Login LoginPage.Model
    | Logout
    | Preferences PreferencesPage.Model
    | Resources ResourcesPage.Model
    | Resource ResourcePage.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            Session.Session Nothing url key Time.utc
    in
    ( Model session (Navbar.init session) Loading, Task.perform SetTimeZone Time.here )



-- UPDATE


type Msg
    = NoOp ()
    | SetTimeZone Time.Zone
    | InitApp (Maybe User.User)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavMsg Navbar.Msg
    | LoginMsg LoginPage.Msg
    | HomeMsg HomePage.Msg
    | PreferencesMsg PreferencesPage.Msg
    | ResourcesMsg ResourcesPage.Msg
    | ResourceMsg ResourcePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SetTimeZone timeZone, _ ) ->
            ( { model | session = Session.setTimeZone model.session timeZone }, Command.send (InitApp Dummy.user) )

        ( InitApp maybeUser, _ ) ->
            let
                session =
                    Session.setUser model.session maybeUser

                navigationCommand =
                    navigateTo session
            in
            ( { model | session = session, navbar = Navbar.init session }, navigationCommand )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        session =
                            Session.setUrl model.session url

                        navigationCommand =
                            navigateTo session
                    in
                    ( { model | session = session }, navigationCommand )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                session =
                    Session.setUrl model.session url

                ( page, command ) =
                    loadPage session
            in
            ( { model | session = session, page = page }, command )

        ( NavMsg navMsg, _ ) ->
            let
                ( newModel, newMsg ) =
                    Navbar.update navMsg model.navbar
            in
            ( { model | navbar = newModel }, Cmd.map NavMsg newMsg )

        ( LoginMsg pageMsg, Login pageModel ) ->
            updatePage Login pageModel LoginMsg pageMsg LoginPage.update model

        ( HomeMsg pageMsg, Home pageModel ) ->
            updatePage Home pageModel HomeMsg pageMsg HomePage.update model

        ( PreferencesMsg pageMsg, Preferences pageModel ) ->
            updatePage Preferences pageModel PreferencesMsg pageMsg PreferencesPage.update model

        ( ResourcesMsg pageMsg, Resources pageModel ) ->
            updatePage Resources pageModel ResourcesMsg pageMsg ResourcesPage.update model

        ( ResourceMsg pageMsg, Resource pageModel ) ->
            updatePage Resource pageModel ResourceMsg pageMsg ResourcePage.update model

        ( _, _ ) ->
            ( model, Cmd.none )


navigateTo : Session.Session -> Cmd Msg
navigateTo session =
    Nav.pushUrl session.key (Url.toString session.url)


loadPage : Session.Session -> ( Page, Cmd Msg )
loadPage session =
    case Route.router session of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.Home ->
            let
                ( pageModel, pageMsg ) =
                    HomePage.init
            in
            ( Home pageModel, Cmd.map HomeMsg pageMsg )

        Route.Login ->
            let
                ( pageModel, pageMsg ) =
                    LoginPage.init
            in
            ( Login pageModel, Cmd.map LoginMsg pageMsg )

        Route.Logout ->
            ( Logout, Cmd.none )

        Route.Preferences ->
            let
                ( pageModel, pageMsg ) =
                    PreferencesPage.init session.maybeUser
            in
            ( Preferences pageModel, Cmd.map PreferencesMsg pageMsg )

        Route.Resources page size ->
            let
                ( pageModel, pageMsg ) =
                    ResourcesPage.init session page size
            in
            ( Resources pageModel, Cmd.map ResourcesMsg pageMsg )

        Route.Resource id ->
            let
                ( pageModel, pageMsg ) =
                    ResourcePage.init session id
            in
            ( Resource pageModel, Cmd.map ResourceMsg pageMsg )


updatePage : (p -> Page) -> p -> (a -> Msg) -> a -> (a -> p -> ( p, Cmd a )) -> Model -> ( Model, Cmd Msg )
updatePage toPage pageModel toMsg pageMsg updateFn model =
    let
        ( newPageModel, newPageMsg ) =
            updateFn pageMsg pageModel
    in
    ( { model | page = toPage newPageModel }, Cmd.map toMsg newPageMsg )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm SPA"
    , body =
        [ Navbar.view model.navbar |> Html.map NavMsg
        , div [ class "section", onClick (NavMsg Navbar.Close) ]
            [ div [ class "container" ]
                [ pageView model
                ]
            ]
        ]
    }


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Loading ->
            LoadingPage.view
                |> Html.map NoOp

        NotFound ->
            NotFoundPage.view
                |> Html.map NoOp

        Home pageModel ->
            HomePage.view pageModel
                |> Html.map HomeMsg

        Login pageModel ->
            LoginPage.view pageModel
                |> Html.map LoginMsg

        Logout ->
            LogoutPage.view
                |> Html.map NoOp

        Preferences pageModel ->
            PreferencesPage.view pageModel
                |> Html.map PreferencesMsg

        Resources pageModel ->
            ResourcesPage.view pageModel
                |> Html.map ResourcesMsg

        Resource pageModel ->
            ResourcePage.view pageModel
                |> Html.map ResourceMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
