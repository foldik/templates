module NewMeetingForm exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Dummy.Users as Users
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Session
import Task
import Time
import Url
import User



-- MODEL


type alias Model =
    { session : Session.Session
    , time : Time.Posix
    , meeting : Meeting
    , formValidation : FormValidation
    , userSearchText : String
    , users : List Users.User
    , saveResult : Maybe (Result String String)
    }


type alias Meeting =
    { title : String
    , description : String
    , date : String
    , from : String
    , to : String
    , participiants : List Users.User
    }


type alias FormValidation =
    { title : Validation
    , participiants : Validation
    }


type Validation
    = Valid
    | Invalid String
    | NotValidated


init : Session.Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , time = Time.millisToPosix 0
            , meeting = emptyMeeting
            , formValidation = emptyFormValidation
            , userSearchText = ""
            , users = Users.list
            , saveResult = Nothing
            }
    in
    ( model, setTime )


setTime : Cmd Msg
setTime =
    Task.perform SetTime Time.now


emptyMeeting : Meeting
emptyMeeting =
    { title = ""
    , description = ""
    , date = ""
    , from = ""
    , to = ""
    , participiants = []
    }


emptyFormValidation : FormValidation
emptyFormValidation =
    { title = NotValidated
    , participiants = NotValidated
    }



-- UPDATE


type Msg
    = SetTime Time.Posix
    | UpdateTitle String
    | UpdateDescription String
    | UpdateDate String
    | UpdateFrom String
    | UpdateTo String
    | SearchParticipiant String
    | AddParticipiant Users.User
    | RemoveParticipiant Users.User
    | SaveMeeting
    | CloseResultMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            model.session

        meeting =
            model.meeting

        formValidation =
            model.formValidation
    in
    case msg of
        SetTime time ->
            let
                newMeeting =
                    { meeting | date = toDate model.session.timeZone time, from = toTime model.session.timeZone time, to = toTime model.session.timeZone time }
            in
            ( { model | meeting = newMeeting, time = time }, Cmd.none )

        UpdateTitle value ->
            let
                newMeeting =
                    { meeting | title = value }

                newFormValidation =
                    { formValidation | title = NotValidated }
            in
            ( { model | meeting = newMeeting, formValidation = newFormValidation }, Cmd.none )

        UpdateDescription value ->
            let
                newMeeting =
                    { meeting | description = value }
            in
            ( { model | meeting = newMeeting }, Cmd.none )

        UpdateDate value ->
            let
                newMeeting =
                    { meeting | date = value }
            in
            ( { model | meeting = newMeeting }, Cmd.none )

        UpdateFrom value ->
            let
                newMeeting =
                    { meeting | from = value }
            in
            ( { model | meeting = newMeeting }, Cmd.none )

        UpdateTo value ->
            let
                newMeeting =
                    { meeting | to = value }
            in
            ( { model | meeting = newMeeting }, Cmd.none )

        SearchParticipiant value ->
            ( { model | userSearchText = value }, Cmd.none )

        AddParticipiant user ->
            let
                participiants =
                    meeting.participiants ++ [ user ]

                newMeeting =
                    { meeting | participiants = participiants }

                users =
                    List.filter (\u -> not (u.id == user.id)) model.users

                newFormValidation =
                    { formValidation | participiants = NotValidated }
            in
            ( { model | meeting = newMeeting, formValidation = newFormValidation, users = users, userSearchText = "" }, Cmd.none )

        RemoveParticipiant user ->
            let
                participiants =
                    List.filter (\u -> not (user.id == u.id)) meeting.participiants

                newMeeting =
                    { meeting | participiants = participiants }

                users =
                    [ user ] ++ model.users
            in
            ( { model | meeting = newMeeting, users = users, userSearchText = "" }, Cmd.none )

        SaveMeeting ->
            let
                validationResult =
                    validateForm meeting
            in
            case valid validationResult of
                True ->
                    let
                        saveResult =
                            Just (Ok ("Successfully created '" ++ meeting.title ++ "' meeting"))
                    in
                    ( { model | meeting = emptyMeeting, formValidation = emptyFormValidation, users = Users.list, userSearchText = "", saveResult = saveResult }, setTime )

                False ->
                    let
                        saveResult =
                            Just (Err "Couldn't save the meeting")
                    in
                    ( { model | formValidation = validationResult, saveResult = saveResult }, Cmd.none )

        CloseResultMessage ->
            ( { model | saveResult = Nothing }, Cmd.none )


toDate : Time.Zone -> Time.Posix -> String
toDate zone time =
    let
        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time

        day =
            Time.toDay zone time
    in
    String.fromInt year ++ "-" ++ toMonthNumber month ++ "-" ++ String.fromInt day


toTime : Time.Zone -> Time.Posix -> String
toTime zone time =
    let
        hour =
            Time.toHour zone time

        minute =
            Time.toMinute zone time
    in
    padLeftTime (String.fromInt hour) ++ ":" ++ padLeftTime (String.fromInt minute)


toMonthNumber : Time.Month -> String
toMonthNumber month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"

padLeftTime : String -> String
padLeftTime value =
    String.padLeft 2 '0' value


validateForm : Meeting -> FormValidation
validateForm meeting =
    let
        title =
            case String.isEmpty (String.trim meeting.title) of
                True ->
                    Invalid "Give me a title"

                False ->
                    Valid

        participiants =
            case List.isEmpty meeting.participiants of
                True ->
                    Invalid "Add here at least one particpiant"

                False ->
                    Valid
    in
    { title = title
    , participiants = participiants
    }


valid : FormValidation -> Bool
valid formValidation =
    List.all (\getter -> isValidParam (getter formValidation))
        [ .title, .participiants ]


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
                    [ text "New Meeting" ]
                , div []
                    [ label [ required True ]
                        [ text "Title:" ]
                    ]
                , div []
                    [ input [ type_ "text", placeholder "Title", onInput UpdateTitle, value model.meeting.title ]
                        []
                    ]
                , div []
                    [ validationView model.formValidation.title ]
                , div []
                    [ label []
                        [ text ("Description: (500/" ++ String.fromInt (String.length model.meeting.description) ++ ")") ]
                    ]
                , div []
                    [ textarea [ onInput UpdateDescription, maxlength 500, value model.meeting.description ]
                        []
                    ]
                , div []
                    [ label []
                        [ text "Time:" ]
                    ]
                , div []
                    [ input [ type_ "date", onInput UpdateDate, value model.meeting.date ]
                        []
                    , input [ type_ "time", onInput UpdateFrom, value model.meeting.from ]
                        []
                    , input [ type_ "time", onInput UpdateTo, value model.meeting.to ]
                        []
                    ]
                , div []
                    [ label []
                        [ text "Participiants:" ]
                    ]
                , div []
                    [ validationView model.formValidation.participiants ]
                , div []
                    (List.map selectedUser model.meeting.participiants)
                , div []
                    [ input [ type_ "text", placeholder "Start typeing", value model.userSearchText, onInput SearchParticipiant ]
                        []
                    ]
                , div []
                    [ text "Users:" ]
                , div []
                    (List.map selectableUser (getMatchedUsers model.userSearchText model.users))
                , div []
                    [ button [ type_ "button", onClick SaveMeeting ]
                        [ text "Save" ]
                    ]
                ]
            ]
        , saveResultView model.saveResult
        ]


validationView : Validation -> Html Msg
validationView validation =
    case validation of
        NotValidated ->
            text ""

        Valid ->
            text ""

        Invalid reason ->
            span [ class "error" ] [ text reason ]


getMatchedUsers : String -> List Users.User -> List Users.User
getMatchedUsers searchText users =
    let
        preparedSearchValue =
            String.toLower (String.trim searchText)
    in
    case String.length preparedSearchValue of
        0 ->
            users

        _ ->
            List.filter (\user -> filterUsers preparedSearchValue user) users


filterUsers : String -> Users.User -> Bool
filterUsers value user =
    let
        firstName =
            String.toLower user.firstName

        lastName =
            String.toLower user.lastName
    in
    case ( String.startsWith value firstName, String.startsWith value lastName ) of
        ( True, _ ) ->
            True

        ( _, True ) ->
            True

        _ ->
            False


selectedUser : Users.User -> Html Msg
selectedUser user =
    span []
        [ strong []
            [ text (user.firstName ++ " " ++ user.lastName) ]
        , button [ type_ "button", onClick (RemoveParticipiant user) ]
            [ text "Remove" ]
        ]


selectableUser : Users.User -> Html Msg
selectableUser user =
    div []
        [ strong []
            [ text (user.firstName ++ " " ++ user.lastName) ]
        , a [ href "/users" ]
            [ text ("@" ++ user.userName) ]
        , button [ type_ "button", onClick (AddParticipiant user) ]
            [ text "Add" ]
        ]


saveResultView : Maybe (Result String String) -> Html Msg
saveResultView result =
    case result of
        Just value ->
            case value of
                Ok message ->
                    p [ class "success" ]
                        [ text message
                        , button [ onClick CloseResultMessage ]
                            [ text "X" ]
                        ]

                Err message ->
                    p [ class "error" ]
                        [ text message
                        , button [ onClick CloseResultMessage ]
                            [ text "X" ]
                        ]

        Nothing ->
            span [] []
