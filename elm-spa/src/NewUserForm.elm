module NewUserForm exposing (Model, Msg, init, update, view)

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
    , userReq : UserRequest
    , validation : FormValidation
    }


type alias UserRequest =
    { firstName : String
    , lastName : String
    , email : String
    , birthDay : String
    }


type alias FormValidation =
    { firstName : Validation
    , lastName : Validation
    , email : Validation
    , birthDay : Validation
    }


type Validation
    = NotValidated
    | Valid
    | Invalid String


init : Session.Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , userReq = emptyUserReq
            , validation = emptyFormValidation
            }
    in
    ( model, Cmd.none )


emptyUserReq : UserRequest
emptyUserReq =
    { firstName = ""
    , lastName = ""
    , email = ""
    , birthDay = ""
    }


emptyFormValidation : FormValidation
emptyFormValidation =
    { firstName = NotValidated
    , lastName = NotValidated
    , email = NotValidated
    , birthDay = NotValidated
    }



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdateBirthDay String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        userReq =
            model.userReq
    in
    case msg of
        UpdateFirstName value ->
            ( { model | userReq = { userReq | firstName = value } }, Cmd.none )

        UpdateLastName value ->
            ( { model | userReq = { userReq | lastName = value } }, Cmd.none )

        UpdateEmail value ->
            ( { model | userReq = { userReq | email = value } }, Cmd.none )

        UpdateBirthDay value ->
            ( { model | userReq = { userReq | birthDay = value } }, Cmd.none )

        Submit ->
            let
                result =
                    validate model.userReq
            in
            case valid result of
                True ->
                    ( { model | userReq = emptyUserReq, validation = emptyFormValidation }, Cmd.none )

                False ->
                    ( { model | validation = result }, Cmd.none )


validate : UserRequest -> FormValidation
validate userReq =
    let
        firstName =
            notBlank userReq.firstName

        lastName =
            notBlank userReq.lastName

        email =
            notBlank userReq.email

        birthDay =
            notBlank userReq.birthDay
    in
    { firstName = firstName
    , lastName = lastName
    , email = email
    , birthDay = birthDay
    }


notBlank : String -> Validation
notBlank value =
    case String.isEmpty (String.trim value) of
        True ->
            Invalid "Mandatory field"

        False ->
            Valid


valid : FormValidation -> Bool
valid formValidation =
    List.all (\getter -> isValidParam (getter formValidation))
        [ .firstName, .lastName, .email, .birthDay ]


isValidParam : Validation -> Bool
isValidParam validation =
    case validation of
        Valid ->
            True

        NotValidated ->
            False

        Invalid _ ->
            False



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
                    [ input [ type_ "text", placeholder "First name", value model.userReq.firstName, onInput UpdateFirstName, autofocus True ]
                        []
                    , validationView model.validation.firstName
                    ]
                , div []
                    [ label []
                        [ text "Last name:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Last name", value model.userReq.lastName, onInput UpdateLastName ]
                        []
                    , validationView model.validation.lastName
                    ]
                , div []
                    [ label []
                        [ text "Email:" ]
                    ]
                , div []
                    [ input [ type_ "email", placeholder "email@example.com", value model.userReq.email, onInput UpdateEmail ]
                        []
                    , validationView model.validation.email
                    ]
                , div []
                    [ label []
                        [ text "Birthday:" ]
                    ]
                , div []
                    [ input [ type_ "date", value model.userReq.birthDay, onInput UpdateBirthDay ]
                        []
                    , validationView model.validation.birthDay
                    ]
                , div []
                    [ button [ onClick Submit, type_ "button" ]
                        [ text "Save" ]
                    ]
                ]
            ]
        ]


validationView : Validation -> Html Msg
validationView validation =
    case validation of
        NotValidated ->
            span [] []

        Valid ->
            span [] []

        Invalid reason ->
            div [ class "error" ]
                [ text reason ]
