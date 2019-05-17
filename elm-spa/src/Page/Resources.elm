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
    , notification : Maybe (Result String String)
    , newResourceForm : NewResourceForm
    , resources : List ResourceApi.Resource
    }


type alias NewResourceForm =
    { isActive : Bool
    , resourceName : String
    }


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
    Model session Nothing initFormData []


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
            ( { model | notification = Nothing }, Cmd.none )

        CreateResourceFormMsg formMsg ->
            let
                ( newResourceForm, notification, command ) =
                    updateCreateForm formMsg model.newResourceForm
            in
            ( { model | newResourceForm = newResourceForm, notification = notification }, Cmd.map CreateResourceFormMsg command )

        GotResources result ->
            case result of
                Ok resources ->
                    ( { model | resources = resources }, Cmd.none )

                Err _ ->
                    ( { model | notification = Just (Err "Couldn't get resources") }, Cmd.none )


updateCreateForm : FormMsg -> NewResourceForm -> ( NewResourceForm, Maybe (Result String String), Cmd FormMsg )
updateCreateForm msg newResourceForm =
    case msg of
        Open ->
            ( { newResourceForm | isActive = True }, Nothing, Cmd.none )

        Close ->
            ( initFormData, Nothing, Cmd.none )

        UpdateResourceName value ->
            ( { newResourceForm | resourceName = value }, Nothing, Cmd.none )

        CreateResource ->
            let
                newResource =
                    ResourceApi.NewResource newResourceForm.resourceName
            in
            ( newResourceForm, Nothing, ResourceApi.createResource CreatedResurce newResource )

        CreatedResurce result ->
            case result of
                Ok resource ->
                    ( initFormData, Just (Ok "Succesfully saved resource"), Cmd.none )

                Err _ ->
                    ( { newResourceForm | isActive = False }, Just (Err "Error happened during saving resource"), Cmd.none )



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


notificationView : Maybe (Result String String) -> Html Msg
notificationView notification =
    case notification of
        Just result ->
            case result of
                Ok value ->
                    div [ class "notification is-success" ]
                        [ button [ class "delete", onClick CloseNotification ] []
                        , text value
                        ]

                Err err ->
                    div [ class "notification is-danger" ]
                        [ button [ class "delete", onClick CloseNotification ] []
                        , text err
                        ]

        Nothing ->
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
