module Page.Resources exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Route



-- MODEL


type alias Model =
    { session : Session.Session
    , notification : Notification
    , newResourceForm : NewResourceForm
    , resources : List ResourceApi.Resource
    }


type alias NewResourceForm =
    { isActive : Bool
    , resourceName : String
    }


type Notification
    = Success String Bool
    | Error String Bool
    | Empty


init : Session.Session -> Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init session maybePageNumber maybePageSize =
    let
        pageLoad =
            toPageLoad maybePageNumber maybePageSize
    in
    case pageLoad of
        Load page size ->
            ( initModel session, ResourceApi.getResources GotResources )

        Reload page size ->
            let
                path =
                    Route.toLink (Route.Resources (Just page) (Just size))

                cmd =
                    Nav.pushUrl session.key path
            in
            ( initModel session, cmd )


initModel : Session.Session -> Model
initModel session =
    Model session Empty initFormData []


initFormData : NewResourceForm
initFormData =
    NewResourceForm False ""


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
    = CloseNotification
    | CreateResourceFormMsg FormMsg
    | GotResources (Result Http.Error (List ResourceApi.Resource))


type FormMsg
    = Open
    | Close
    | UpdateResourceName String
    | CreateResource
    | CreatedResurce (Result Http.Error ResourceApi.Resource)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseNotification ->
            ( { model | notification = Empty }, Cmd.none )

        CreateResourceFormMsg formMsg ->
            let
                ( newResourceForm, notification, command ) =
                    updateCreateForm formMsg model.newResourceForm
            in
            case notification of
                Success message isReload ->
                    if isReload then
                        ( { model | newResourceForm = newResourceForm, notification = notification }, ResourceApi.getResources GotResources )

                    else
                        ( { model | newResourceForm = newResourceForm, notification = notification }, Cmd.none )

                Error message isReload ->
                    if isReload then
                        ( { model | newResourceForm = newResourceForm, notification = notification }, ResourceApi.getResources GotResources )

                    else
                        ( { model | newResourceForm = newResourceForm, notification = notification }, Cmd.none )

                _ ->
                    ( { model | newResourceForm = newResourceForm, notification = notification }, Cmd.map CreateResourceFormMsg command )

        GotResources result ->
            case result of
                Ok resources ->
                    ( { model | resources = resources }, Cmd.none )

                Err _ ->
                    ( { model | notification = Error "Couldn't get resources" False }, Cmd.none )


updateCreateForm : FormMsg -> NewResourceForm -> ( NewResourceForm, Notification, Cmd FormMsg )
updateCreateForm msg newResourceForm =
    case msg of
        Open ->
            ( { newResourceForm | isActive = True }, Empty, Cmd.none )

        Close ->
            ( initFormData, Empty, Cmd.none )

        UpdateResourceName value ->
            ( { newResourceForm | resourceName = value }, Empty, Cmd.none )

        CreateResource ->
            let
                newResource =
                    ResourceApi.NewResource newResourceForm.resourceName
            in
            ( newResourceForm, Empty, ResourceApi.createResource CreatedResurce newResource )

        CreatedResurce result ->
            case result of
                Ok resource ->
                    ( initFormData, Success "Succesfully saved resource" True, Cmd.none )

                Err _ ->
                    ( { newResourceForm | isActive = False }, Error "Error happened during saving resource" True, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ notificationView model.notification
        , h1 [ class "title is-1" ]
            [ text "Resources" ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ button [ class "button is-primary is-pulled-right", onClick (CreateResourceFormMsg Open) ]
                    [ text "New" ]
                ]
            ]
        , Html.map CreateResourceFormMsg (newResourceModal model.newResourceForm)
        , resourcesView model.resources
        ]


notificationView : Notification -> Html Msg
notificationView notification =
    case notification of
        Success message _ ->
            div [ class "notification is-success" ]
                [ button [ class "delete", onClick CloseNotification ] []
                , text message
                ]

        Error message _ ->
            div [ class "notification is-danger" ]
                [ button [ class "delete", onClick CloseNotification ] []
                , text message
                ]

        Empty ->
            div [] []


newResourceModal : NewResourceForm -> Html FormMsg
newResourceModal newResourceForm =
    div [ classList [ ( "modal", True ), ( "is-active", newResourceForm.isActive ) ] ]
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
                        [ input [ class "input", type_ "text", placeholder "Name of the resource", value newResourceForm.resourceName, onInput UpdateResourceName ]
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


resourcesView : List ResourceApi.Resource -> Html Msg
resourcesView resources =
    div []
        (List.map
            (\resource ->
                div [ class "card" ]
                    [ div [ class "card-header" ]
                        [ div [ class "card-header-title" ]
                            [ text resource.name ]
                        ]
                    ]
            )
            resources
        )
