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
    }


type alias NewMeeting =
    { title : Forms.Value String
    , description : Forms.Value String
    , date : Forms.Value String
    , from : Forms.Value String
    , to : Forms.Value String
    , participiants : List (Forms.Value String)
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session emptyNewMeeting, Cmd.none )


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
            ]
        ]
