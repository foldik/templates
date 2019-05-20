module Page.Resources exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Array
import Browser.Navigation as Nav
import Components.CardTable as CardTable
import Components.Notification as Notification
import Components.Pagination as Pagination
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model.Session as Session
import Page.NewResourceModal as NewResourceModal
import Route
import Utils.Lists



-- MODEL


type alias Model =
    { session : Session.Session
    , notification : Notification.Notification
    , newResourceModal : NewResourceModal.Model
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
    Model session Notification.Empty (NewResourceModal.init session) []


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
    = NoOp ()
    | CloseNotification
    | CreateResourceFormMsg NewResourceModal.Msg
    | GotResources (Result Http.Error (List ResourceApi.Resource))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        CloseNotification ->
            ( { model | notification = Notification.Empty }, Cmd.none )

        CreateResourceFormMsg formMsg ->
            let
                ( newResourceModal, command ) =
                    NewResourceModal.update formMsg model.newResourceModal
            in
            ( { model | newResourceModal = newResourceModal }, Cmd.map CreateResourceFormMsg command )

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
        , div []
            [ h1 [ class "title is-1" ]
                [ text "Resources" ]
            ]
        , div [ class "columns has-padding-bottom-20" ]
            [ div [ class "column" ]
                [ button [ class "button is-link is-pulled-right", onClick (CreateResourceFormMsg NewResourceModal.Open) ]
                    [ text "New" ]
                ]
            ]
        , Html.map CreateResourceFormMsg (NewResourceModal.view model.newResourceModal)
        , Pagination.view (Pagination.Pagination 10 10 40) |> Html.map NoOp
        , div [] [ CardTable.view 4 model.resources resourceCardView ]
        , Pagination.view (Pagination.Pagination 10 10 40) |> Html.map NoOp
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


resourceCardView : ResourceApi.Resource -> Html Msg
resourceCardView resource =
    a [ href (Route.toLink (Route.Resource resource.id)) ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ div [ class "card-header-title" ]
                    [ h1 [ class "title" ]
                        [ text resource.name ]
                    ]
                ]
            ]
        ]
