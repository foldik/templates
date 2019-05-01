module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Task
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , maybeUser : Maybe User
    , page : Page
    }


type Page
    = Loading
    | NotFound
    | Home HomeModel
    | Page Int


type alias HomeModel =
    Maybe Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model url key Nothing Loading, send (InitPage (Just adminUser)) )


adminUser : User
adminUser =
    User "Adrienn" Admin


simpleUser : User
simpleUser =
    User "Kristóf" SimpleUser


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity



-- UPDATE


type Msg
    = InitPage (Maybe User)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GoToRoute Route.Route
    | HomePageMsg HomeMsg


type HomeMsg
    = Increase
    | Decrease


type User
    = User String Role


getRole : User -> Role
getRole (User name role) =
    role


type Role
    = Admin
    | SimpleUser


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( InitPage maybeUser, _ ) ->
            ( { model | maybeUser = maybeUser }, Nav.pushUrl model.key (Url.toString model.url) )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | url = url, page = loadPage url model.maybeUser }, Cmd.none )

        ( GoToRoute route, _ ) ->
            ( model, Nav.pushUrl model.key (Route.toString route) )

        ( HomePageMsg pageMsg, Home pageModel ) ->
            ( model, Cmd.none )

        {-
           case pageMsg of
               Increase ->
                   ({model | page = Home (pageModel + 1)}, Cmd.none)

               Decrease ->
                   ({model | page = Home (pageModel - 1)}, Cmd.none)
        -}
        ( _, _ ) ->
            ( model, Cmd.none )


loadPage : Url.Url -> Maybe User -> Page
loadPage url maybeUser =
    case authorize (Route.router url) maybeUser of
        Route.NotFound ->
            NotFound

        Route.Home ->
            Home Nothing

        Route.Page id ->
            Page id


authorize : Route.Route -> Maybe User -> Route.Route
authorize route maybeUser =
    let
        authPredicate =
            authConfig route
    in
    case authPredicate maybeUser of
        True ->
            route

        False ->
            Route.NotFound


authConfig : Route.Route -> (Maybe User -> Bool)
authConfig route =
    case route of
        Route.NotFound ->
            allow

        Route.Home ->
            hasAnyRole [ Admin, SimpleUser ]

        Route.Page id ->
            hasAnyRole [ Admin ]


allow : Maybe User -> Bool
allow maybeUser =
    True


hasAnyRole : List Role -> (Maybe User -> Bool)
hasAnyRole allowedRoles =
    \maybeUser ->
        case maybeUser of
            Nothing ->
                False

            Just user ->
                List.member (getRole user) allowedRoles



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm SPA"
    , body =
        [ navBar model
        , div [ class "section" ]
            [ div [ class "container" ]
                [ pageView model
                ]
            ]
        ]
    }


navBar : Model -> Html Msg
navBar model =
    div [ class "container" ]
        [ a [ href "/", class "button" ]
            [ text "Home" ]
        , a [ href "/page/1", class "button" ]
            [ text "/page/1" ]
        , a [ href "/page/2", class "button" ]
            [ text "/page/2" ]
        ]


pageView : Model -> Html Msg
pageView model =
    case model.page of
        Loading ->
            h1 [ class "title is-1" ]
                [ text "Loading" ]

        NotFound ->
            h1 [ class "title is-1" ]
                [ text "Not found" ]

        Home id ->
            div []
                [ h1 [ class "title is-1" ]
                    [ text "Home" ]
                , h1 [ class "subtitle is-3" ]
                    [ text ("Value: " ++ String.fromInt 1) ]
                , button [ class "button", onClick Increase ]
                    [ text "Increase" ]
                , button [ class "button", onClick Decrease ]
                    [ text "Decrease" ]

                {- }                , button [ class "button", onClick (GoToRoute (Route.Page id))]
                   [ text ("/page/" ++ (String.fromInt id)) ]
                -}
                ]
                |> Html.map HomePageMsg

        Page id ->
            h1 [ class "title is-1" ]
                [ text ("Page " ++ String.fromInt id) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
