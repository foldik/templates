module Page.Resources exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
    { value : Int
    }


init : Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init page size =
    ( Model 10, Cmd.none )



-- UPDATE


type Msg
    = Increase


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | value = model.value + 1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    h1 [ class "title is-1" ]
        [ text ("Page " ++ String.fromInt model.value) ]
