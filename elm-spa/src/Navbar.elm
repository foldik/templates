module Navbar exposing (Model, Msg, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Role as Role
import Model.User as User



-- MODEL


type alias Model =
    { user : Maybe User.User
    , isOpen : Bool
    }



-- UPDATE


type Msg
    = Toggle
    | Close



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ a [ href "/", class "button" ]
            [ text "Home" ]
        , a [ href "/page/1", class "button" ]
            [ text "/page/1" ]
        , a [ href "/page/2", class "button" ]
            [ text "/page/2" ]
        ]
