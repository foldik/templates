module Page.Resource exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Page.NotFound as NotFoundPage
import Url



-- MODEL


type alias Model =
    { session : Session.Session
    , resource : Loadable
    }


type Loadable
    = Loading Int
    | Loaded EditableResource
    | NotFound


type alias EditableResource =
    { id : Int
    , name : Editable String
    , shortDescription : Editable String
    , timestamp : Int
    }


type Editable a
    = InEdit a
    | InSaved a


init : Session.Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( Model session (Loading id), ResourceApi.getResource id GotResourceResponse )



-- UPDATE


type Msg
    = NoOp ()
    | GotResourceResponse (Result Http.Error ResourceApi.Resource)
    | EditTitle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        GotResourceResponse result ->
            case result of
                Ok resource ->
                    let
                        editableResource =
                            EditableResource resource.id (InSaved resource.name) (InSaved resource.shortDescription) resource.timestamp
                    in
                    ( { model | resource = Loaded editableResource }, Cmd.none )

                Err _ ->
                    ( { model | resource = NotFound }, Cmd.none )

        EditTitle ->
            case model.resource of
                Loading id ->
                    ( model, Cmd.none )

                NotFound ->
                    ( model, Cmd.none )

                Loaded editableResource ->
                    ( { model | resource = Loaded { editableResource | name = toEdited editableResource.name } }, Cmd.none )


toEdited : Editable a -> Editable a
toEdited editable =
    case editable of
        InSaved value ->
            InEdit value

        InEdit value ->
            InEdit value



-- VIEW


view : Model -> Html Msg
view model =
    case model.resource of
        Loading id ->
            h1 [ class "title is-1" ]
                [ text "Loading" ]

        NotFound ->
            Html.map NoOp NotFoundPage.view

        Loaded resource ->
            div []
                [ case resource.name of
                    InSaved value ->
                        h1 [ class "title is-1", onClick EditTitle ]
                            [ text value ]

                    InEdit value ->
                        textarea [ class "textarea title is-1 has-padding-5", rows 1, autofocus True ]
                            [ text value ]
                ]
