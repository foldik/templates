module Page.Resources exposing (Model, Msg, init, update, view)

import Api.Resources as ResourceApi
import Array
import Browser.Navigation as Nav
import Components.Notification as Notification
import Components.PaginatedCardList as PaginatedCardList
import DateTime
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
    , resources : ResourceApi.PaginatedList ResourceApi.Resource
    }


init : Session.Session -> Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init session maybePageNumber maybePageLimit =
    let
        pageLoad =
            toPageLoad maybePageNumber maybePageLimit
    in
    case pageLoad of
        Load page limit ->
            ( initModel page limit session, ResourceApi.getResources page limit GotResources )

        Reload page limit ->
            let
                path =
                    Route.toLink (Route.Resources (Just page) (Just limit))

                cmd =
                    Nav.pushUrl session.key path
            in
            ( initModel page limit session, cmd )


initModel : Int -> Int -> Session.Session -> Model
initModel page limit session =
    Model session Notification.Empty (NewResourceModal.init session) (ResourceApi.PaginatedList page limit 0 [])


type PageLoad
    = Load Int Int
    | Reload Int Int


toPageLoad : Maybe Int -> Maybe Int -> PageLoad
toPageLoad maybePageNumber maybePageLimit =
    case ( maybePageNumber, maybePageLimit ) of
        ( Just page, Just limit ) ->
            Load page limit

        ( Just page, Nothing ) ->
            Reload page 10

        ( Nothing, Just limit ) ->
            Reload 1 limit

        ( Nothing, Nothing ) ->
            Reload 1 10



-- UPDATE


type Msg
    = NoOp ()
    | CloseNotification
    | CreateResourceFormMsg NewResourceModal.Msg
    | GotResources (Result Http.Error (ResourceApi.PaginatedList ResourceApi.Resource))


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
        , PaginatedCardList.view (paginatedViewConfig model.session model.resources)
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


paginatedViewConfig : Session.Session -> ResourceApi.PaginatedList ResourceApi.Resource -> PaginatedCardList.Model ResourceApi.Resource Msg
paginatedViewConfig session paginatedList =
    { page = paginatedList.page
    , limit = paginatedList.limit
    , count = paginatedList.count
    , items = paginatedList.data
    , itemView = resourceCardView session
    , hrefFunc = hrefFunc
    }


resourceCardView : Session.Session -> (ResourceApi.Resource -> Html Msg)
resourceCardView session =
    \resource ->
        a [ href (Route.toLink (Route.Resource resource.id)) ]
            [ div [ class "card" ]
                [ div [ class "card-content" ]
                    [ h1 [ class "title" ]
                        [ text resource.name ]
                    , p [ class "subtitle is-6" ]
                        [ text (DateTime.toyyyyMMddhhmm resource.timestamp session.timeZone) ]
                    , p [ class "content" ]
                        [ text resource.shortDescription ]
                    ]
                ]
            ]


hrefFunc : Int -> Int -> String
hrefFunc page limit =
    Route.toLink (Route.Resources (Just page) (Just limit))
