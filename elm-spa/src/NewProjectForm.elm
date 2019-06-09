module NewProjectForm exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
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
    , projectName : FormValue String
    , url : FormValue String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session (formParam "" [ notEmpty ]) (formParam "" [ notEmpty ]), Cmd.none )



-- Form / Validation


type alias FormValue a =
    ( a, List (a -> ValidationResult), ValidationResult )


formParam : a -> List (a -> ValidationResult) -> FormValue a
formParam value validations =
    ( value, validations, Valid )


getValue : FormValue a -> a
getValue ( value, _, _ ) =
    value


updateValue : a -> FormValue a -> FormValue a
updateValue value ( _, validations, validationResult ) =
    ( value, validations, Valid )


validate : FormValue a -> FormValue a
validate ( value, validations, _ ) =
    let
        validationResult =
            List.map (\rule -> rule value) validations
                |> List.map
                    (\result ->
                        case result of
                            Invalid reason ->
                                reason

                            Valid ->
                                ""
                    )
                |> List.filter
                    (\message ->
                        case message of
                            "" ->
                                False

                            innerMessage ->
                                True
                    )
    in
    if List.isEmpty validationResult then
        ( value, validations, Valid )

    else
        ( value, validations, Invalid (String.join "" validationResult) )


notEmpty : String -> ValidationResult
notEmpty =
    \value ->
        if String.length value == 0 then
            Invalid "Must not be empty"

        else
            Valid


type ValidationResult
    = Valid
    | Invalid String



-- UPDATE


type Msg
    = UpdateProjectName String
    | UpdateUrl String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateProjectName value ->
            ( { model | projectName = updateValue value model.projectName }, Cmd.none )

        UpdateUrl value ->
            ( { model | url = updateValue value model.url }, Cmd.none )

        Submit ->
            let
                projectName =
                    validate model.projectName

                url =
                    validate model.url
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
                    [ input [ type_ "text", placeholder "Project name", value (getValue model.projectName), onInput UpdateProjectName, autofocus True ]
                        []
                    , validationView model.projectName
                    ]
                , div []
                    [ label []
                        [ text "Url:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "url", value (getValue model.url), onInput UpdateUrl, autofocus True ]
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


validationView : FormValue a -> Html Msg
validationView ( _, _, validationResult ) =
    case validationResult of
        Valid ->
            span [] []

        Invalid reason ->
            p []
                [ text reason ]
