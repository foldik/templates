module Components.Pagination exposing (Pagination, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MODEL


type alias Pagination =
    { page : Int
    , limit : Int
    , max : Int
    }



-- VIEW


view : Pagination -> Html ()
view pagination =
    if pagination.max < 2 then
        span [] []

    else
        let
            previousButton =
                if [ pagination.page == 1, pagination.max == 0 ] |> List.any (\n -> n == True) then
                    button [ class "pagination-previous button", disabled True ]
                        [ text "Previous" ]

                else
                    a [ href "", class "pagination-previous button" ]
                        [ text "Previous" ]

            nextButton =
                if pagination.page == pagination.max then
                    button [ class "pagination-next button", disabled True ]
                        [ text "Next page" ]

                else
                    a [ href "", class "pagination-next button" ]
                        [ text "Next page" ]

            pageButtons =
                pageButtonsView pagination
        in
        nav
            [ class "pagination is-centered has-padding-bottom-20 has-padding-top-20"
            , attribute "role" "navigation"
            , attribute "aria-label" "pagination"
            ]
            [ previousButton
            , nextButton
            , pageButtons
            ]


pageButtonsView : Pagination -> Html ()
pageButtonsView pagination =
    ul [ class "pagination-list" ]
        (pageButtonsList pagination
            |> List.map (\n -> pageButtonView n pagination)
        )


pageButtonsList : Pagination -> List String
pageButtonsList pagination =
    if pagination.max < 1 then
        []

    else if pagination.max <= 5 then
        List.range 1 pagination.max
            |> List.map (\n -> String.fromInt n)

    else if pagination.page < 4 then
        [ "1", "2", "3", "4", "DOTS", String.fromInt pagination.max ]

    else if pagination.page > (pagination.max - 3) then
        [ "1", "DOTS", String.fromInt (pagination.max - 3), String.fromInt (pagination.max - 2), String.fromInt (pagination.max - 1), String.fromInt pagination.max ]

    else
        [ "1", "DOTS", String.fromInt (pagination.page - 1), String.fromInt pagination.page, String.fromInt (pagination.page + 1), "DOTS", String.fromInt pagination.max ]


pageButtonView : String -> Pagination -> Html ()
pageButtonView pageString pagination =
    case pageString of
        "DOTS" ->
            li []
                [ span [ class "pagination-ellipsis" ]
                    [ text "..." ]
                ]

        _ ->
            li []
                [ a
                    [ href ""
                    , classList [ ( "pagination-link button", True ), ( "is-current", pageString == String.fromInt pagination.page ) ]
                    , attribute "aria-label" ("Goto page " ++ pageString)
                    ]
                    [ text pageString ]
                ]
