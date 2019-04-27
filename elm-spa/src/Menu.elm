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
    , conditions : Session.Session -> Bool
    , subMenuItems : List SubMenuItem
    }


type alias SubMenuItem =
    { name : String
    , link : String
    , conditions : Session.Session -> Bool
    }


config : List MenuItem
config =
    [ MenuItem "Login"
        "/login"
        Session.notSignedIn
        []
    , MenuItem "Logout"
        "/logout"
        Session.isSignedIn
        []
    , MenuItem "File"
        "/"
        (Session.hasAnyRole [ Role.Admin, Role.User ])
        [ SubMenuItem "Save"
            "/save"
            always
        , SubMenuItem "Edit"
            "/edit"
            (Session.hasAnyRole [ Role.Admin ])
        ]
    ]


always : Session.Session -> Bool
always =
    \session -> True



-- VIEW


view : msg -> Bool -> Session.Session -> Html msg
view msg isOpen session =
    div []
        [ text "Menu" ]


boolToString : Bool -> String
boolToString value =
    case value of
        True ->
            "true"

        False ->
            "false"
