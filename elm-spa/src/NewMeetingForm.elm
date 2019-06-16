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
    , newMeeting : NewMeeting
    , userSearchText : String
    , users : List Users.User
    }


type alias NewMeeting =
    { title : String
    , description : String
    , date : String
    , from : String
    , to : String
    , participiants : List Users.User
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , time = Time.millisToPosix 0
            , newMeeting = emptyNewMeeting
            , userSearchText = ""
            , users = Users.list
            }
    in
    ( model, Task.perform GetTime Time.now )


emptyNewMeeting : NewMeeting
emptyNewMeeting =
    { title = ""
    , description = ""
    , date = ""
    , from = ""
    , to = ""
    , participiants = []
    }



-- UPDATE


type Msg
    = GetTime Time.Posix
    | UpdateTitle String
    | UpdateDescription String
    | UpdateDate String
    | UpdateFrom String
    | UpdateTo String
    | SearchParticipiant String
    | AddParticipiant Users.User
    | RemoveParticipiant Users.User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTime time ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | date = toDate model.session.timeZone time, from = toTime model.session.timeZone time, to = toTime model.session.timeZone time }
            in
            ( { model | newMeeting = newMeeting, time = time }, Cmd.none )

        UpdateTitle value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | title = value }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateDescription value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | description = value }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateDate value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | date = value }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateFrom value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | from = value }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateTo value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | to = value }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        SearchParticipiant value ->
            ( { model | userSearchText = value }, Cmd.none )

        AddParticipiant user ->
            let
                meeting =
                    model.newMeeting

                participiants =
                    meeting.participiants ++ [ user ]

                newMeeting =
                    { meeting | participiants = participiants }

                users =
                    List.filter (\u -> not (u.id == user.id)) model.users
            in
            ( { model | newMeeting = newMeeting, users = users, userSearchText = "" }, Cmd.none )

        RemoveParticipiant user ->
            let
                meeting =
                    model.newMeeting

                participiants =
                    List.filter (\u -> not (user.id == u.id)) meeting.participiants

                newMeeting =
                    { meeting | participiants = participiants }

                users =
                    [ user ] ++ model.users
            in
            ( { model | newMeeting = newMeeting, users = users, userSearchText = "" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.form []
        [ fieldset []
            [ legend []
                [ text "New Meeting" ]
            , div []
                [ label []
                    [ text "Title:" ]
                ]
            , div []
                [ input [ type_ "text", placeholder "Title", onInput UpdateTitle ]
                    []
                ]
            , div []
                [ label []
                    [ text "Description:" ]
                ]
            , div []
                [ textarea [ onInput UpdateDescription ]
                    []
                ]
            , div []
                [ label []
                    [ text "Time:" ]
                ]
            , div []
                [ input [ type_ "date", onInput UpdateDate, value model.newMeeting.date ]
                    []
                , input [ type_ "time", onInput UpdateFrom, value model.newMeeting.from ]
                    []
                , input [ type_ "time", onInput UpdateTo, value model.newMeeting.to ]
                    []
                ]
            , div []
                [ label []
                    [ text "Participiants:" ]
                ]
            , div []
                (List.map selectedUser model.newMeeting.participiants)
            , div []
                [ input [ type_ "text", placeholder "Start typeing", value model.userSearchText, onInput SearchParticipiant ]
                    []
                ]
            , div []
                [ text "Users:" ]
            , div []
                (List.map selectableUser (getMatchedUsers model.userSearchText model.users))
            , div []
                [ button [ type_ "button" ]
                    [ text "Save" ]
                ]
            ]
        ]


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
    String.fromInt hour ++ ":" ++ String.fromInt minute


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
