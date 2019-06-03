module Components.PaginatedCardList exposing (Model, view)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils.Lists



-- MODEL


type alias Model a msg =
    { page : Int
    , limit : Int
    , count : Int
    , items : List a
    , itemView : a -> Html msg
    , hrefFunc : Int -> Int -> String
    }



-- VIEW


view : Model a msg -> Html msg
view model =
    div []
        [ pagingView model
        , cardTable 3 model.items model.itemView
        , pagingView model
        ]



-- PAGING


pagingView : Model a msg -> Html msg
pagingView pagination =
    if pagination.count < 2 then
        span [] []

    else
        let
            pageButtons =
                pageButtonsView pagination
        in
        nav
            [ class "pagination is-centered has-padding-bottom-20 has-padding-top-20"
            , attribute "role" "navigation"
            , attribute "aria-label" "pagination"
            ]
            [ pageButtons
            ]


pageButtonsView : Model a msg -> Html msg
pageButtonsView pagination =
    ul [ class "pagination-list" ]
        (pageButtonsList pagination
            |> List.map (\n -> pageButtonView n pagination)
        )


type PaginationButton
    = Number Int
    | Dots
    | Previous
    | Next


pageButtonsList : Model a msg -> List PaginationButton
pageButtonsList pagination =
    let
        buttons =
            if pagination.count < 1 then
                []

            else if pagination.count <= 5 then
                List.range 1 pagination.count
                    |> List.map (\n -> Number n)

            else if pagination.page < 4 then
                [ Number 1, Number 2, Number 3, Number 4, Dots, Number pagination.count ]

            else if pagination.page > (pagination.count - 3) then
                [ Number 1, Dots, Number (pagination.count - 3), Number (pagination.count - 2), Number (pagination.count - 1), Number pagination.count ]

            else
                [ Number 1, Dots, Number (pagination.page - 1), Number pagination.page, Number (pagination.page + 1), Dots, Number pagination.count ]
    in
    case ( pagination.page > 1, pagination.page < pagination.count ) of
        ( True, True ) ->
            (Previous :: buttons) ++ [ Next ]

        ( True, False ) ->
            Previous :: buttons

        ( False, True ) ->
            buttons ++ [ Next ]

        ( False, False ) ->
            buttons


pageButtonView : PaginationButton -> Model a msg -> Html msg
pageButtonView paginationButton pagination =
    case paginationButton of
        Previous ->
            li []
                [ a
                    [ href (pagination.hrefFunc (pagination.page - 1) pagination.limit)
                    , class "pagination-link button"
                    ]
                    [ text (String.fromChar '←') ]
                ]

        Next ->
            li []
                [ a
                    [ href (pagination.hrefFunc (pagination.page + 1) pagination.limit)
                    , class "pagination-link button"
                    ]
                    [ text (String.fromChar '→') ]
                ]

        Dots ->
            li []
                [ span [ class "pagination-ellipsis" ]
                    [ text "..." ]
                ]

        Number page ->
            li []
                [ a
                    [ href (pagination.hrefFunc page pagination.limit)
                    , classList [ ( "pagination-link button", True ), ( "is-current", page == pagination.page ) ]
                    , attribute "aria-label" ("Goto page " ++ String.fromInt page)
                    ]
                    [ text (String.fromInt page) ]
                ]



-- CARDS


cardTable : Int -> List a -> (a -> Html msg) -> Html msg
cardTable numberOfColumns items viewFunc =
    let
        rows =
            Utils.Lists.split items numberOfColumns
    in
    div [] (List.map (\row -> rowView numberOfColumns row viewFunc) rows)


rowView : Int -> List a -> (a -> Html msg) -> Html msg
rowView numberOfColumns items viewFunc =
    let
        iteamsAsArray =
            Array.fromList items
    in
    div [ class "columns" ]
        (List.map
            (\idx ->
                case Array.get idx iteamsAsArray of
                    Just item ->
                        div [ class "column" ]
                            [ viewFunc item
                            ]

                    Nothing ->
                        div [ class "column" ] []
            )
            (List.range 0 (numberOfColumns - 1))
        )
