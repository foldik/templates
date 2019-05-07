module Page.Resources exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Model =
    { value : Int
    , pageLoad : PageLoad
    }


init : Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init maybePageNumber maybePageSize =
    let
        pageLoad =
            toPageLoad maybePageNumber maybePageSize
    in
    ( Model 10 pageLoad, Cmd.none )


type PageLoad
    = Load Int Int
    | Reload Int Int


toPageLoad : Maybe Int -> Maybe Int -> PageLoad
toPageLoad maybePageNumber maybePageSize =
    case ( maybePageNumber, maybePageSize ) of
        ( Just page, Just size ) ->
            Load page size

        ( Just page, Nothing ) ->
            Reload page 10

        ( Nothing, Just size ) ->
            Reload 1 size

        ( Nothing, Nothing ) ->
            Reload 1 10



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
