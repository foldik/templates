module Page.Logout exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.User as User



-- MODEL
-- UPDATE
-- VIEW


view : Html ()
view =
    h1 [ class "title is-1" ]
        [ text "Logout" ]
