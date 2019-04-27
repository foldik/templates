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
    { isMenuOpen : Bool
    , session : Session.Session
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
    ( Model False session
    , Api.loadSession GotSession
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ToggleMenu
    | GotSession (Result Http.Error User.User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

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
        [ headerView model.isMenuOpen model.session
        ]
    }


headerView : Bool -> Session.Session -> Html Msg
headerView isMenuOpen session =
    let
        menuView =
            case session of
                Session.NotSignedIn _ _ _ ->
                    [ div [ class "navbar-start" ]
                        [ a [ class "navbar-item", href "/login" ] [ text "Login" ]
                        ]
                    ]

                Session.SignedIn _ _ _ user ->
                    [ div [ class "navbar-start" ] (roleBasedMenuView user)
                    , div [ class "navbar-end" ]
                        [ div [ class "navbar-item" ]
                            [ div [ class "buttons" ]
                                [ a [ class "button is-light", href "/logout" ]
                                    [ text "Logout" ]
                                ]
                            ]
                        ]
                    ]
    in
    nav [ class "navbar is-link", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "/" ] [ strong [] [ text "╦╣" ] ]
            , span
                [ onClick ToggleMenu
                , class "navbar-burger burger"
                , attribute "role" "button"
                , attribute "aria-label" "menu"
                , attribute "aria-expanded" (boolToString isMenuOpen)
                , attribute "data-target" "navItems"
                ]
                [ span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                , span [ attribute "aria-hidden" "true" ] []
                ]
            ]
        , div [ id "navItems", classList [ ( "navbar-menu", True ), ( "is-active", isMenuOpen ) ] ]
            menuView
        ]


roleBasedMenuView : User.User -> List (Html Msg)
roleBasedMenuView user =
    case user.role of
        Role.Admin ->
            [ a [ class "navbar-item", href "/" ] [ strong [] [ text "Home" ] ]
            ]

        Role.User ->
            [ a [ class "navbar-item", href "/" ] [ strong [] [ text "Home" ] ]
            ]


boolToString : Bool -> String
boolToString value =
    case value of
        True ->
            "true"

        False ->
            "false"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
