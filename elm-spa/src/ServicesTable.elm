module ServicesTable exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Dummy.Services as Services
import Forms
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session
import Time
import Url
import User



-- MODEL


type alias Model =
    { session : Session.Session
    , services : List Services.Service
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session Services.list, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "Name" ]
                , th []
                    [ text "Version" ]
                , th []
                    [ text "State" ]
                , th []
                    [ text "Number of instances" ]
                , th []
                    [ text "Url" ]
                ]
            ]
        , tbody []
            (List.map serviceRowView model.services)
        ]


serviceRowView : Services.Service -> Html Msg
serviceRowView service =
    tr []
        [ td []
            [ text service.name ]
        , td []
            [ text service.version ]
        , td []
            [ text (Services.stateToString service.state) ]
        , td []
            [ text (String.fromInt service.instances) ]
        , td []
            [ a [ href service.url ] [ text (String.fromChar '↗') ] ]
        ]
