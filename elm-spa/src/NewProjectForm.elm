module NewProjectForm exposing (Model, Msg, init, update, view)

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
    , projectName : Forms.Value String
    , url : Forms.Value String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session (Forms.formParam "" [ Forms.notEmpty ]) (Forms.formParam "" [ Forms.notEmpty ]), Cmd.none )



-- UPDATE


type Msg
    = UpdateProjectName String
    | UpdateUrl String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateProjectName value ->
            ( { model | projectName = Forms.setValue value model.projectName }, Cmd.none )

        UpdateUrl value ->
            ( { model | url = Forms.setValue value model.url }, Cmd.none )

        Submit ->
            let
                projectName =
                    Forms.validate model.projectName

                url =
                    Forms.validate model.url
            in
            ( { model | projectName = projectName, url = url }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form []
            [ fieldset []
                [ legend []
                    [ text "New Project" ]
                , div []
                    [ label []
                        [ text "Project name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Project name", value (Forms.getValue model.projectName), onInput UpdateProjectName, autofocus True ]
                        []
                    , validationView model.projectName
                    ]
                , div []
                    [ label []
                        [ text "Url:" ]
                    ]
                , div []
                    [ input [ type_ "url", placeholder "url", value (Forms.getValue model.url), onInput UpdateUrl, autofocus True ]
                        []
                    , validationView model.url
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
