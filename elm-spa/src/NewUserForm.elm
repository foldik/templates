module NewUserForm exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Forms
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session
import Time
import Url
import User



-- MODEL


type alias Model =
    { session : Session.Session
    , firstName : Forms.Value String
    , lastName : Forms.Value String
    , email : Forms.Value String
    , birthDay : Forms.Value String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session (Forms.formParam "" [ Forms.notEmpty ]) (Forms.formParam "" [ Forms.notEmpty ]) (Forms.formParam "" [ Forms.notEmpty ]) (Forms.formParam "" [ Forms.notEmpty ]), Cmd.none )



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdateBirthDay String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFirstName value ->
            ( { model | firstName = Forms.setValue value model.firstName }, Cmd.none )

        UpdateLastName value ->
            ( { model | lastName = Forms.setValue value model.lastName }, Cmd.none )

        UpdateEmail value ->
            ( { model | email = Forms.setValue value model.email }, Cmd.none )

        UpdateBirthDay value ->
            ( { model | birthDay = Forms.setValue value model.birthDay }, Cmd.none )

        Submit ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form []
            [ fieldset []
                [ legend []
                    [ text "New User" ]
                , div []
                    [ label []
                        [ text "First name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "First name", value (Forms.getValue model.firstName), onInput UpdateFirstName, autofocus True ]
                        []
                    , validationView model.firstName
                    ]
                , div []
                    [ label []
                        [ text "Last name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Last name", value (Forms.getValue model.lastName), onInput UpdateLastName ]
                        []
                    , validationView model.lastName
                    ]
                , div []
                    [ label []
                        [ text "Email:" ]
                    ]
                , div []
                    [ input [ type_ "email", placeholder "email@example.com", value (Forms.getValue model.email), onInput UpdateEmail ]
                        []
                    , validationView model.email
                    ]
                , div []
                    [ label []
                        [ text "Birthday:" ]
                    ]
                , div []
                    [ input [ type_ "date", value (Forms.getValue model.birthDay), onInput UpdateBirthDay ]
                        []
                    , validationView model.birthDay
                    ]
                , div []
                    [ button [ onClick Submit, type_ "button" ]
                        [ text "Save" ]
                    ]
                ]
            ]
        ]


validationView : Forms.Value a -> Html Msg
validationView ( _, _, validationResult ) =
    case validationResult of
        Forms.Valid ->
            span [] []

        Forms.Invalid reason ->
            p []
                [ text reason ]
