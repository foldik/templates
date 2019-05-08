module Page.Resources exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Session as Session
import Route



-- MODEL


type alias Model =
    { session : Session.Session
    , pageLoad : PageLoad
    }


init : Session.Session -> Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init session maybePageNumber maybePageSize =
    let
        pageLoad =
            toPageLoad maybePageNumber maybePageSize
    in
    case pageLoad of
        Load page size ->
            ( Model session pageLoad, Cmd.none )

        Reload page size ->
            let
                path =
                    Route.toLink (Route.Resources (Just page) (Just size))

                cmd =
                    Nav.pushUrl session.key path
            in
            ( Model session pageLoad, cmd )


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
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    h1 [ class "title is-1" ]
        [ text "Page " ]
