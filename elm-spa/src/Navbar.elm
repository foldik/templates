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
    | Dropdown String (List SubNavItem)
    | Button Route.Route


type SubNavItem
    = SubNavItem Route.Route


menu : NavConfig
menu =
    NavConfig
        [ NavItem (Route.Resources 10 Nothing)
        , Dropdown "More"
            [ SubNavItem (Route.Resources 2 Nothing)
            , SubNavItem (Route.Resources 3 Nothing)
            ]
        ]


init : Maybe User.User -> Model
init maybeUser =
    Model maybeUser False (filterAuthorized maybeUser menu)


filterAuthorized : Maybe User.User -> NavConfig -> NavConfig
filterAuthorized maybeUser navConfig =
    let
        start =
            navConfig.start
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
                    navBarItemView route

                Dropdown name subNavItems ->
                    div [ class "navbar-item has-dropdown is-hoverable" ]
                        [ span [ class "navbar-link" ]
                            [ text name ]
                        , div [ class "navbar-dropdown" ]
                            (List.map (\(SubNavItem route) -> navBarItemView route) subNavItems)
                        ]

                _ ->
                    div [] []
        )
        navConfig.start


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
