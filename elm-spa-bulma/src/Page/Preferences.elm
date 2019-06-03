module Page.Preferences exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.User as User



-- MODEL


type alias Model =
    { maybeUser : Maybe User.User
    }


init : Maybe User.User -> ( Model, Cmd Msg )
init maybeUser =
    ( Model maybeUser, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    h1 [ class "title is-1" ]
        [ text "Preferences" ]
