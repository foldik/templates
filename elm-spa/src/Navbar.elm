module Navbar exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Role as Role
import Model.User as User
import Route



-- MODEL


type alias Model =
    { user : Maybe User.User
    , isOpen : Bool
    , config : NavConfig
    }


type alias NavConfig =
    { start : List NavItem }


type NavItem
    = NavItem Route.Route
    | Dropdown (List SubNavItem)
    | Button Route.Route


type SubNavItem
    = SubNavItem Route.Route


menu : NavConfig
menu =
    NavConfig
        [ NavItem (Route.Resources 10 Nothing)
        ]


init : Maybe User.User -> Model
init maybeUser =
    Model maybeUser False (filterAuthorized maybeUser menu)


filterAuthorized : Maybe User.User -> NavConfig -> NavConfig
filterAuthorized maybeUser navConfig =
    let
        start =
            List.filter
                (\navItem ->
                    case navItem of
                        NavItem route ->
                            Route.authorized route maybeUser

                        _ ->
                            True
                )
                navConfig.start
    in
    NavConfig start



-- UPDATE


type Msg
    = Toggle
    | Close


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            let
                isOpen =
                    not model.isOpen
            in
            ( { model | isOpen = isOpen }, Cmd.none )

        Close ->
            ( { model | isOpen = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    nav [ class "navbar is-dark", attribute "role" "navigation", attribute "aria-label" "main navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "/" ]
                [ strong []
                    [ text "╦╣" ]
                ]
            , hamburgerButton model
            ]
        , div [ id "navItems", classList [ ( "navbar-menu", True ), ( "is-active", model.isOpen ) ] ]
            [ div [ class "navbar-start" ]
                (navStartView model.config)
            ]
        ]


hamburgerButton : Model -> Html Msg
hamburgerButton model =
    span
        [ onClick Toggle
        , class "navbar-burger burger"
        , attribute "role" "button"
        , attribute "aria-label" "menu"
        , attribute "aria-expanded" (boolToString model.isOpen)
        , attribute "data-target" "navItems"
        ]
        [ span [ attribute "aria-hidden" "true" ] []
        , span [ attribute "aria-hidden" "true" ] []
        , span [ attribute "aria-hidden" "true" ] []
        ]


navStartView : NavConfig -> List (Html Msg)
navStartView navConfig =
    List.map
        (\navItem ->
            case navItem of
                NavItem route ->
                    a [ class "navbar-item", href (Route.toLink route), onClick Close ] [ text (Route.toString route) ]

                _ ->
                    div [] []
        )
        navConfig.start


boolToString : Bool -> String
boolToString value =
    case value of
        True ->
            "true"

        False ->
            "false"
