module Page.Resources exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Notification as Notification
import Model.Session as Session
import Page.NewResourceModal as NewResourceModal
import Route
import Utils.Lists



-- MODEL


type alias Model =
    { session : Session.Session
    , notification : Notification.Notification
    , newResourceForm : NewResourceModal.Model
    , resources : List ResourceApi.Resource
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
    Model session Notification.Empty NewResourceModal.init []


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
    | CreateResourceFormMsg NewResourceModal.Msg
    | GotResources (Result Http.Error (List ResourceApi.Resource))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseNotification ->
            ( { model | notification = Notification.Empty }, Cmd.none )

        CreateResourceFormMsg formMsg ->
            let
                ( newResourceForm, notification, command ) =
                    NewResourceModal.update formMsg model.newResourceForm
            in
            case notification of
                Notification.Success message isReload ->
                    if isReload then
                        ( { model | newResourceForm = newResourceForm, notification = notification }, ResourceApi.getResources GotResources )

                    else
                        ( { model | newResourceForm = newResourceForm, notification = notification }, Cmd.none )

                Notification.Error message isReload ->
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
                    ( { model | notification = Notification.Error "Couldn't get resources" False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ notificationView model.notification
        , h1 [ class "title is-1" ]
            [ text "Resources" ]
        , div [ class "columns" ]
            [ div [ class "column" ]
                [ button [ class "button is-primary is-pulled-right", onClick (CreateResourceFormMsg NewResourceModal.Open) ]
                    [ text "New" ]
                ]
            ]
        , Html.map CreateResourceFormMsg (NewResourceModal.view model.newResourceForm)
        , resourcesTable model.resources
        ]


notificationView : Notification.Notification -> Html Msg
notificationView notification =
    case notification of
        Notification.Success message _ ->
            div [ class "notification is-success" ]
                [ button [ class "delete", onClick CloseNotification ] []
                , text message
                ]

        Notification.Error message _ ->
            div [ class "notification is-danger" ]
                [ button [ class "delete", onClick CloseNotification ] []
                , text message
                ]

        Notification.Empty ->
            div [] []


resourcesTable : List ResourceApi.Resource -> Html Msg
resourcesTable resources =
    let
        splittedResources =
            Utils.Lists.split resources 3
    in
    div []
        (List.map
            (\resourceRow ->
                div [ class "columns" ]
                    (List.map
                        (\resourceColumn ->
                            div [ class "column" ]
                                [ div [ class "card" ]
                                    [ div [ class "card-header" ]
                                        [ div [ class "card-header-title" ]
                                            [ text resourceColumn.name ]
                                        ]
                                    ]
                                ]
                        )
                        resourceRow
                    )
            )
            splittedResources
        )
