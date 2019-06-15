module NewMeetingForm exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Dummy.Users as Users
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
    , newMeeting : NewMeeting
    , userSearchText : String
    , suggestedParticipiants : List Users.User
    }


type alias NewMeeting =
    { title : Forms.Value String
    , description : Forms.Value String
    , date : Forms.Value String
    , from : Forms.Value String
    , to : Forms.Value String
    , participiants : List Users.User
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , newMeeting = emptyNewMeeting
            , userSearchText = ""
            , suggestedParticipiants = []
            }
    in
    ( model, Cmd.none )


emptyNewMeeting : NewMeeting
emptyNewMeeting =
    { title = Forms.formParam "" [ Forms.notEmpty ]
    , description = Forms.formParam "" [ Forms.notEmpty ]
    , date = Forms.formParam "" [ Forms.notEmpty ]
    , from = Forms.formParam "" [ Forms.notEmpty ]
    , to = Forms.formParam "" [ Forms.notEmpty ]
    , participiants = []
    }



-- UPDATE


type Msg
    = UpdateTitle String
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
        UpdateTitle value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | title = Forms.setValue value meeting.title }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateDescription value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | description = Forms.setValue value meeting.description }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateDate value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | date = Forms.setValue value meeting.date }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateFrom value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | from = Forms.setValue value meeting.from }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        UpdateTo value ->
            let
                meeting =
                    model.newMeeting

                newMeeting =
                    { meeting | to = Forms.setValue value meeting.to }
            in
            ( { model | newMeeting = newMeeting }, Cmd.none )

        SearchParticipiant value ->
            let
                preparedSearchValue =
                    String.toLower (String.trim value)

                suggestedParticipiants =
                    case String.length preparedSearchValue of
                        0 ->
                            []

                        _ ->
                            List.filter (\user -> filterUsers preparedSearchValue user) Users.list
            in
            ( { model | suggestedParticipiants = suggestedParticipiants }, Cmd.none )

        AddParticipiant user ->
            let
                meeting =
                    model.newMeeting

                participiants =
                    meeting.participiants ++ [ user ]

                newMeeting =
                    { meeting | participiants = participiants }
            in
            ( { model | newMeeting = newMeeting, suggestedParticipiants = [] }, Cmd.none )

        RemoveParticipiant user ->
            let
                meeting =
                    model.newMeeting

                participiants =
                    List.filter (\u -> not (user.id == u.id)) meeting.participiants

                newMeeting =
                    { meeting | participiants = participiants }
            in
            ( { model | newMeeting = newMeeting, suggestedParticipiants = [] }, Cmd.none )


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
                [ input [ type_ "date", onInput UpdateDate ]
                    []
                , input [ type_ "time", onInput UpdateFrom ]
                    []
                , input [ type_ "time", onInput UpdateTo ]
                    []
                ]
            , div []
                [ label []
                    [ text "Participiants:" ]
                ]
            , div []
                [ selectedParticipiants model.newMeeting.participiants ]
            , div []
                [ input [ type_ "text", placeholder "Start typeing", onInput SearchParticipiant ]
                    []
                ]
            , div []
                [ particpiantsSelector model.suggestedParticipiants ]
            ]
        ]


selectedParticipiants : List Users.User -> Html Msg
selectedParticipiants users =
    div []
        (List.map selectedUser users)


selectedUser : Users.User -> Html Msg
selectedUser user =
    span []
        [ strong []
            [ text (user.firstName ++ " " ++ user.lastName) ]
        , button [ type_ "button", onClick (RemoveParticipiant user) ]
            [ text "Remove" ]
        ]


particpiantsSelector : List Users.User -> Html Msg
particpiantsSelector users =
    div []
        (List.map selectableUser users)


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
