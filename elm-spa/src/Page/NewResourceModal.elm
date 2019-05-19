module Page.NewResourceModal exposing (Model, Msg(..), init, update, view)

import Api.Resources as ResourceApi
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Notification as Notification



-- MODEL


type alias Model =
    { isActive : Bool
    , resourceName : String
    }


init : Model
init =
    Model False ""



-- UPDATE


type Msg
    = Open
    | Close
    | UpdateResourceName String
    | CreateResource
    | CreatedResurce (Result Http.Error ResourceApi.Resource)


update : Msg -> Model -> ( Model, Notification.Notification, Cmd Msg )
update msg model =
    case msg of
        Open ->
            ( { model | isActive = True }, Notification.Empty, Cmd.none )

        Close ->
            ( init, Notification.Empty, Cmd.none )

        UpdateResourceName value ->
            ( { model | resourceName = value }, Notification.Empty, Cmd.none )

        CreateResource ->
            let
                newResource =
                    ResourceApi.NewResource model.resourceName
            in
            ( model, Notification.Empty, ResourceApi.createResource CreatedResurce newResource )

        CreatedResurce result ->
            case result of
                Ok resource ->
                    ( init, Notification.Success "Succesfully saved resource" True, Cmd.none )

                Err _ ->
                    ( { model | isActive = False }, Notification.Error "Error happened during saving resource" True, Cmd.none )



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
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button is-primary", onClick CreateResource ]
                    [ text "Save" ]
                , button [ class "button", onClick Close ]
                    [ text "Cancel" ]
                ]
            ]
        ]
