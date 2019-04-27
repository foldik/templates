module Menu exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Role
import Session
import User



-- MODEL


type alias MenuItem =
    { name : String
    , link : String
    , conditions : List (Session.Session -> Bool)
    , subMenuItems : List SubMenuItem
    }


type alias SubMenuItem =
    { name : String
    , link : String
    , conditions : List (Session.Session -> Bool)
    }


config : List MenuItem
config =
    [ MenuItem "Login"
        "/login"
        [ Session.notSignedIn ]
        []
    , MenuItem "Logout"
        "/logout"
        [ Session.isSignedIn ]
        []
    , MenuItem "Home"
        "/"
        [ hasAnyRole [ Role.Admin, Role.User ] ]
        []
    ]


hasAnyRole : List Role.Role -> (Session.Session -> Bool)
hasAnyRole roles =
    \session ->
        case session of
            Session.NotSignedIn _ _ _ ->
                False

            Session.SignedIn _ _ _ user ->
                List.any (\role -> role == user.role) roles



-- VIEW


view : msg -> Session.Session -> List MenuItem -> Html msg
view msg session menuItems =
    div []
        [ text "Menu" ]
