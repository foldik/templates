module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Command
import Dummy
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Role as Role
import Model.User as User
import Navbar
import Page.Home as HomePage
import Page.Loading as LoadingPage
import Page.NotFound as NotFoundPage
import Page.Preferences as PreferencesPage
import Page.Resources as ResourcesPage
import Route
import Task
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
    { url : Url.Url
    , key : Nav.Key
    , maybeUser : Maybe User.User
    , navbar : Navbar.Model
    , page : Page
    }


type Page
    = Loading
    | NotFound
    | Home HomePage.Model
    | Preferences PreferencesPage.Model
    | Resources ResourcesPage.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        maybeUser =
            Just Dummy.adminUser
    in
    ( Model url key Nothing (Navbar.init Nothing) Loading, Command.send (InitApp maybeUser) )



-- UPDATE


type Msg
    = NoOp ()
    | InitApp (Maybe User.User)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavMsg Navbar.Msg
    | HomeMsg HomePage.Msg
    | PreferencesMsg PreferencesPage.Msg
    | ResourcesMsg ResourcesPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( InitApp maybeUser, _ ) ->
            ( { model | maybeUser = maybeUser, navbar = Navbar.init maybeUser }, Nav.pushUrl model.key (Url.toString model.url) )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                ( page, command ) =
                    loadPage url model.maybeUser
            in
            ( { model | url = url, page = page }, command )

        ( NavMsg navMsg, _ ) ->
            let
                ( newModel, newMsg ) =
                    Navbar.update navMsg model.navbar
            in
            ( { model | navbar = newModel }, Cmd.map NavMsg newMsg )

        ( HomeMsg pageMsg, Home pageModel ) ->
            updatePage Home pageModel HomeMsg pageMsg HomePage.update model

        ( PreferencesMsg pageMsg, Preferences pageModel ) ->
            updatePage Preferences pageModel PreferencesMsg pageMsg PreferencesPage.update model

        ( ResourcesMsg pageMsg, Resources pageModel ) ->
            updatePage Resources pageModel ResourcesMsg pageMsg ResourcesPage.update model

        ( _, _ ) ->
            ( model, Cmd.none )


loadPage : Url.Url -> Maybe User.User -> ( Page, Cmd Msg )
loadPage url maybeUser =
    case Route.router url maybeUser of
        Route.NotFound ->
            ( NotFound, Cmd.none )

        Route.Home ->
            let
                ( pageModel, pageMsg ) =
                    HomePage.init
            in
            ( Home pageModel, Cmd.map HomeMsg pageMsg )

        Route.Preferences ->
            let
                ( pageModel, pageMsg ) =
                    PreferencesPage.init maybeUser
            in
            ( Preferences pageModel, Cmd.map PreferencesMsg pageMsg )

        Route.Resources id maybePageNumber ->
            let
                ( pageModel, pageMsg ) =
                    ResourcesPage.init id
            in
            ( Resources pageModel, Cmd.map ResourcesMsg pageMsg )


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

        Preferences pageModel ->
            PreferencesPage.view pageModel
                |> Html.map PreferencesMsg

        Resources pageModel ->
            ResourcesPage.view pageModel
                |> Html.map ResourcesMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
