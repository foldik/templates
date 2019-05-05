module Page.Loading exposing (view)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)

-- MODEL

-- UPDATE

-- VIEW

view : Html ()
view =
    div []
        [ h1 [ class "title is-1" ]
            [ text "Loading" ]
        ]