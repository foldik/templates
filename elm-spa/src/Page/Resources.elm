module Page.Resources exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model.Session as Session
import Route



-- MODEL


type alias Model =
    { session : Session.Session
    , newResourceForm : NewResourceForm
    }


type alias NewResourceForm =
    { isActive : Bool
    }



{-
   Data -> Data
   Data -> ValidatedData
-}


type FormValue a
    = Valid a
    | Invalid String a


init : Session.Session -> Maybe Int -> Maybe Int -> ( Model, Cmd Msg )
init session maybePageNumber maybePageSize =
    let
        pageLoad =
            toPageLoad maybePageNumber maybePageSize
    in
    case pageLoad of
        Load page size ->
            ( Model session (NewResourceForm False), Cmd.none )

        Reload page size ->
            let
                path =
                    Route.toLink (Route.Resources (Just page) (Just size))

                cmd =
                    Nav.pushUrl session.key path
            in
            ( Model session (NewResourceForm False), cmd )


type PageLoad
    = Load Int Int
    | Reload Int Int


toPageLoad : Maybe Int -> Maybe Int -> PageLoad
toPageLoad maybePageNumber maybePageSize =
    case ( maybePageNumber, maybePageSize ) of
        ( Just page, Just size ) ->
            Load page size

        ( Just page, Nothing ) ->
            Reload page 10

        ( Nothing, Just size ) ->
            Reload 1 size

        ( Nothing, Nothing ) ->
            Reload 1 10



-- UPDATE


type Msg
    = NewResourceFormMsg FormMsg


type FormMsg
    = OpenNewResourceForm
    | CloseNewResourceForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewResourceFormMsg formMsg ->
            let
                ( newResourceForm, command ) =
                    updateNewResourceForm formMsg model.newResourceForm
            in
            ( { model | newResourceForm = newResourceForm }, command )


updateNewResourceForm : FormMsg -> NewResourceForm -> ( NewResourceForm, Cmd Msg )
updateNewResourceForm msg newResourceForm =
    case msg of
        OpenNewResourceForm ->
            ( { newResourceForm | isActive = True }, Cmd.none )

        CloseNewResourceForm ->
            ( { newResourceForm | isActive = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title is-1" ]
            [ text "Resources" ]
        , div []
            [ button [ class "button is-primary is-pulled-right", onClick (NewResourceFormMsg OpenNewResourceForm) ]
                [ text "New" ]
            ]
        , Html.map NewResourceFormMsg (newResourceModal model.newResourceForm)
        ]


newResourceModal : NewResourceForm -> Html FormMsg
newResourceModal newResourceForm =
    div [ classList [ ( "modal", True ), ( "is-active", newResourceForm.isActive ) ] ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ text "New resource" ]
                , button [ class "delete", attribute "aria-label" "close", onClick CloseNewResourceForm ]
                    []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "field" ]
                    [ label [ class "label" ]
                        [ text "Name" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", placeholder "Name of the resource" ]
                            []
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ button [ class "button is-primary", onClick CloseNewResourceForm ]
                    [ text "Save" ]
                , button [ class "button", onClick CloseNewResourceForm ]
                    [ text "Cancel" ]
                ]
            ]
        ]
