module Main exposing (Model(..), Msg(..), init, main, pageView, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


type Model
    = Hi
    | Hello


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Hi, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello"
    , body =
        [ pageView model ]
    }


pageView : Model -> Html Msg
pageView model =
    let
        greeting =
            case model of
                Hi ->
                    "Hi"

                Hello ->
                    "Hello"
    in
    div []
        [ nav []
            [ a [ href "/" ] 
                [ text "Logo" ]
            , a [ href "/projects" ] 
                [ text "Projects" ]
            , a [ href "/users" ] 
                [ text "Users" ]
            , input [ type_ "text" ]
                []
            , select []
                [ option [ value "settins" ]
                    [ text "Settings"]
                , option [ value "logout" ] 
                    [ text "Logout" ]
                ]
            ]
        , h1 []
            [ text greeting ]
        , blockquote []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]
        , p []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer aliquam dapibus nibh, vel congue tellus. Aliquam ex ex, placerat ut nibh et, facilisis condimentum odio. Vestibulum orci dolor, eleifend eu dapibus vel, lacinia non diam. Nunc sed dictum diam, quis imperdiet diam. Phasellus imperdiet, urna non pellentesque eleifend, purus dolor commodo mi, eget vulputate dolor ex quis nibh. Quisque porttitor accumsan risus, id maximus urna ornare non. Suspendisse at efficitur lectus, cursus facilisis metus. Nam lorem dui, semper quis elementum id, suscipit nec quam. Ut eget ex tortor. Proin tempor urna a mi varius, in euismod nibh lacinia. Maecenas nec turpis et metus molestie mollis eu vel magna." ]
        , pre []
            [ code [] 
                [ text """view : Model -> Html Msg
view model =
    div []
        [ text model.name ]""" ]
            ] 
        , p []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer aliquam dapibus nibh, vel congue tellus. Aliquam ex ex, placerat ut nibh et, facilisis condimentum odio. Vestibulum orci dolor, eleifend eu dapibus vel, lacinia non diam. Nunc sed dictum diam, quis imperdiet diam. Phasellus imperdiet, urna non pellentesque eleifend, purus dolor commodo mi, eget vulputate dolor ex quis nibh. Quisque porttitor accumsan risus, id maximus urna ornare non. Suspendisse at efficitur lectus, cursus facilisis metus. Nam lorem dui, semper quis elementum id, suscipit nec quam. Ut eget ex tortor. Proin tempor urna a mi varius, in euismod nibh lacinia. Maecenas nec turpis et metus molestie mollis eu vel magna." ]
        , div []
            [ select []
                [ option [ value "auth" ]
                    [ text "auth" ]
                , option [ value "users" ]
                    [ text "users" ]
                ]
            , div []
                [ input [ type_ "date" ]
                    []
                ]
            ]
        , table []
            [ caption []
                [ text "Services" ]
            , thead []
                [ tr [] 
                    [ th [] 
                        [ input [ type_ "checkbox" ]
                            []
                        ]
                    , th [] 
                        [ text "Service" ]
                    , th []
                        [ text "Version" ]
                    , th []
                        [ text "Uptime" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] 
                        [ input [ type_ "checkbox" ]
                            []
                        ]
                    , td []
                        [ text "auth" ]
                    , td []
                        [ text "1.0.17" ]
                    , td []
                        [ text "2 hours" ]
                    ]
                , tr []
                    [ td [] 
                        [ input [ type_ "checkbox" ]
                            []
                        ]
                    , td []
                        [ text "users" ]
                    , td []
                        [ text "2.1.8" ]
                    , td []
                        [ text "7 hours" ]
                    ]
                ]
            ]
        , div []
            [ button [] 
                [ text (String.fromChar '←') ]
            , button [] 
                [ text (String.fromChar '→') ]
            ]
        , Html.form []
            [ fieldset []
                [ legend []
                    [ text "New service" ]
                , div []
                    [ label []
                        [ text "Service name:" ]
                    , input [ type_ "text" ]
                        []
                    ]
                , div []
                    [ label []
                        [ text "Repository:" ]
                    , input [ type_ "url" ]
                        []
                    ]
                , div []
                    [ label []
                        [ text "Tag:" ]
                    , select [ ]
                        [ option [ value "0.1.0" ]
                            [ text "0.1.0" ]
                        , option [ value "0.2.0" ]
                            [ text "0.2.0" ]
                        , option [ value "0.2.1" ]
                            [ text "0.2.1" ]
                        , option [ value "0.2.2" ]
                            [ text "0.2.2" ]
                        ]
                    ]
                , div []
                    [ label []
                        [ text "Environment:" ]
                    , div [] 
                        [ textarea [ rows 7, cols 30 ]
                            []
                        ]
                    ]
                , div []
                    [ label []
                        [ text "Auto scale" ]
                    , input [ type_ "checkbox" ]
                        []
                    ]
                , button [ type_ "button" ]
                    [ text "Create" ]
                ]
            ]

        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
