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
    { start : List NavItem
    , end : List NavItem
    }


type NavItem
    = NavItem Route.Route
    | Dropdown String (List SubNavItem)
    | Button Route.Route


type SubNavItem
    = SubNavItem Route.Route


menu : NavConfig
menu =
    NavConfig
        [ NavItem (Route.Resources Nothing Nothing)
        ]
        [ Dropdown "User Logo"
            [ SubNavItem Route.Preferences
            , SubNavItem Route.Logout
            ]
        , Button Route.Login
        ]


init : Maybe User.User -> Model
init maybeUser =
    let
        navConfig =
            menu
    in
    Model
        maybeUser
        False
        (NavConfig
            (filterAuthorized maybeUser navConfig.start)
            (filterAuthorized maybeUser navConfig.end)
        )


filterAuthorized : Maybe User.User -> List NavItem -> List NavItem
filterAuthorized maybeUser navItems =
    navItems
        |> List.filter
            (\navItem ->
                case navItem of
                    NavItem route ->
                        Route.authorized route maybeUser

                    Button route ->
                        Route.authorized route maybeUser

                    _ ->
                        True
            )
        |> List.map
            (\navItem ->
                case navItem of
                    Dropdown name subNavItems ->
                        Dropdown name (List.filter (\(SubNavItem route) -> Route.authorized route maybeUser) subNavItems)

                    _ ->
                        navItem
            )
        |> List.filter
            (\navItem ->
                case navItem of
                    Dropdown name subNavItems ->
                        not (List.isEmpty subNavItems)

                    _ ->
                        True
            )



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
                (navRegion model.config.start)
            , div [ class "navbar-end" ]
                (navRegion model.config.end)
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


navRegion : List NavItem -> List (Html Msg)
navRegion navItems =
    List.map
        (\navItem ->
            case navItem of
                NavItem route ->
                    navBarItemView route

                Dropdown name subNavItems ->
                    div [ class "navbar-item has-dropdown is-hoverable" ]
                        [ span [ class "navbar-link" ]
                            [ text name ]
                        , div [ class "navbar-dropdown" ]
                            (List.map (\(SubNavItem route) -> navBarItemView route) subNavItems)
                        ]

                Button route ->
                    div [ class "navbar-item" ]
                        [ div [ class "buttons" ]
                            [ a [ class "button is-light", href (Route.toLink route) ]
                                [ text (Route.toString route) ]
                            ]
                        ]
        )
        navItems


navBarItemView : Route.Route -> Html Msg
navBarItemView route =
    a [ class "navbar-item", href (Route.toLink route), onClick Close ] [ text (Route.toString route) ]


boolToString : Bool -> String
boolToString value =
    case value of
        True ->
            "true"

        False ->
            "false"
