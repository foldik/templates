module Form exposing (Model, Msg, init, update, view)

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
    , serviceName : FormValue String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session (formParam "" [ notEmpty ]), Cmd.none )



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
    = UpdateServiceName String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateServiceName value ->
            ( { model | serviceName = updateValue value model.serviceName }, Cmd.none )

        Submit ->
            let
                serviceName =
                    validate model.serviceName
            in
            case serviceName of
                ( _, _, Invalid reason ) ->
                    ( { model | serviceName = serviceName }, Cmd.none )

                ( _, _, Valid ) ->
                    ( { model | serviceName = formParam "" [ notEmpty ] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ modelView model
        , Html.form []
            [ fieldset []
                [ legend []
                    [ text "New Service" ]
                , div []
                    [ label []
                        [ text "Service name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Service name", value (getValue model.serviceName), onInput UpdateServiceName, autofocus True ]
                        []
                    , validationView model.serviceName
                    ]
                , div []
                    [ button [ onClick Submit, type_ "button" ]
                        [ text "Save" ]
                    ]
                ]
            ]
        ]


modelView : Model -> Html Msg
modelView model =
    div []
        [ h1 []
            [ text "Model" ]
        , p []
            [ text ("serviceName: " ++ getValue model.serviceName) ]
        ]


validationView : FormValue a -> Html Msg
validationView ( _, _, validationResult ) =
    case validationResult of
        Valid ->
            span [] []

        Invalid reason ->
            p []
                [ text reason ]
