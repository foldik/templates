module Main exposing (Model, Msg(..), init, main, pageView, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NewMeetingForm
import NewProjectForm
import NewUserForm
import Route
import ServicesTable
import Session
import Task
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
    | NewMeetingFormPage NewMeetingForm.Model
    | ServicesTablePage ServicesTable.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            { user = Nothing, url = url, key = key, timeZone = Time.utc }
    in
    ( { session = session, page = NotFound }, Task.perform AdjustTimeZone Time.here )


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

        Route.NewMeeting ->
            let
                ( pageModel, pageMsg ) =
                    NewMeetingForm.init session
            in
            ( NewMeetingFormPage pageModel, Cmd.map NewMeetingFormMsg pageMsg )

        Route.Services ->
            let
                ( pageModel, pageMsg ) =
                    ServicesTable.init session
            in
            ( ServicesTablePage pageModel, Cmd.map ServicesTableMsg pageMsg )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AdjustTimeZone Time.Zone
    | NewProjectFormMsg NewProjectForm.Msg
    | NewUserFormMsg NewUserForm.Msg
    | NewMeetingFormMsg NewMeetingForm.Msg
    | ServicesTableMsg ServicesTable.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

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

        ( AdjustTimeZone zone, _ ) ->
            let
                oldSession =
                    model.session

                session =
                    { oldSession | timeZone = zone }

                ( page, command ) =
                    routeToPage session (Route.router session.url)
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

        ( NewMeetingFormMsg pageMsg, NewMeetingFormPage pageModel ) ->
            let
                ( newPageModel, newPageMsg ) =
                    NewMeetingForm.update pageMsg pageModel
            in
            ( { model | page = NewMeetingFormPage newPageModel }, Cmd.map NewMeetingFormMsg newPageMsg )

        ( ServicesTableMsg pageMsg, ServicesTablePage pageModel ) ->
            let
                ( newPageModel, newPageMsg ) =
                    ServicesTable.update pageMsg pageModel
            in
            ( { model | page = ServicesTablePage newPageModel }, Cmd.map ServicesTableMsg newPageMsg )

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
        [ navItem Route.Services
        , navItem Route.NewMeeting
        , navItem Route.NewUser
        , navItem Route.NewProject
        ]


navItem : Route.Route -> Html Msg
navItem route =
    a [ href (Route.toLink route) ]
        [ text (Route.toString route)
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

        NewMeetingFormPage pageModel ->
            NewMeetingForm.view pageModel
                |> Html.map NewMeetingFormMsg

        ServicesTablePage pageModel ->
            ServicesTable.view pageModel
                |> Html.map ServicesTableMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
