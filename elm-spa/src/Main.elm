module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    , maybeUser : Maybe User
    , page : Page
    }


type Page
    = Loading
    | NotFound
    | Home
    | Page Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key Nothing Loading, send (InitPage (Just adminUser)) )


adminUser : User
adminUser =
    User "Adrienn" Admin


simpleUser : User
simpleUser =
    User "Kristóf" SimpleUser


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity



-- UPDATE


type Msg
    = InitPage (Maybe User)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type User
    = User String Role


type Role
    = Admin
    | SimpleUser


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitPage maybeUser ->
            ( { model | page = loadPage model.url model.maybeUser }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, page = loadPage url model.maybeUser }, Cmd.none )


loadPage : Url.Url -> Maybe User -> Page
loadPage url maybeUser =
    case authorize (Route.router url) maybeUser of
        Route.NotFound ->
            NotFound

        Route.Home ->
            Home

        Route.Page id ->
            Page id


authorize : Route.Route -> Maybe User -> Route.Route
authorize route maybeUser =
    let
        authPredicate =
            authConfig route
    in
    case authPredicate maybeUser of
        True ->
            route

        False ->
            Route.NotFound


authConfig : Route.Route -> (Maybe User -> Bool)
authConfig route =
    case route of
        Route.NotFound ->
            allow

        Route.Home ->
            allow

        Route.Page id ->
            allow


allow : Maybe User -> Bool
allow maybeUser =
    True



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm SPA"
    , body =
        [ navBar model
        , div [ class "section" ]
            [ div [ class "container" ]
                [ pageView model
                ]
            ]
        ]
    }


navBar : Model -> Html Msg
navBar model =
    div [ class "container" ]
        [ a [ href "/", class "button" ]
            [ text "Home" ]
        , a [ href "/page/1", class "button" ]
            [ text "/page/1" ]
        , a [ href "/page/2", class "button" ]
            [ text "/page/2" ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Loading ->
            h1 [ class "title is-1" ]
                [ text "Loading" ]

        NotFound ->
            h1 [ class "title is-1" ]
                [ text "Not found" ]

        Home ->
            h1 [ class "title is-1" ]
                [ text "Home" ]

        Page id ->
            h1 [ class "title is-1" ]
                [ text ("Page " ++ String.fromInt id) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
