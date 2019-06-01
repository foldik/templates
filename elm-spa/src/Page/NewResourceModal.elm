module Page.NewResourceModal exposing (Model, Msg(..), init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Url



-- MODEL


type alias Model =
    { session : Session.Session
    , isActive : Bool
    , resourceName : String
    , shortDescription : String
    }


init : Session.Session -> Model
init session =
    Model session False "" ""



-- UPDATE


type Msg
    = Open
    | Close
    | UpdateResourceName String
    | UpdateShortDescription String
    | CreateResource
    | CreatedResurce (Result Http.Error ResourceApi.Resource)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Open ->
            ( { model | isActive = True }, Cmd.none )

        Close ->
            ( init model.session, Cmd.none )

        UpdateResourceName value ->
            ( { model | resourceName = value }, Cmd.none )

        UpdateShortDescription value ->
            ( { model | shortDescription = value }, Cmd.none )

        CreateResource ->
            let
                newResource =
                    ResourceApi.NewResource model.resourceName model.shortDescription
            in
            ( model, ResourceApi.createResource CreatedResurce newResource )

        CreatedResurce result ->
            case result of
                Ok resource ->
                    ( init model.session, Nav.pushUrl model.session.key ("/resources/" ++ String.fromInt resource.id) )

                Err _ ->
                    ( { model | isActive = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "modal", True ), ( "is-active", model.isActive ) ] ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ text "New resource" ]
                , button [ class "delete", attribute "aria-label" "close", onClick Close ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "field" ]
                    [ label [ class "label" ]
                        [ text "Name" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", placeholder "Name of the resource", value model.resourceName, onInput UpdateResourceName ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ div [ class "control" ]
                        [ textarea [ class "textarea", placeholder "Short description", onInput UpdateShortDescription ]
                            []
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button is-primary", onClick CreateResource ]
                    [ text "Save" ]
                , button [ class "button", onClick Close ]
                    [ text "Cancel" ]
                ]
            ]
        ]
