module Components.CardTable exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils.Lists


view : Int -> List a -> (a -> Html msg) -> Html msg
view numberOfColumns items viewFunc =
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
