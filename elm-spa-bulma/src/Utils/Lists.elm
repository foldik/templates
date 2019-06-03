module Utils.Lists exposing (split)

import List exposing (..)


split : List a -> Int -> List (List a)
split list chunkSize =
    case take chunkSize list of
        [] ->
            []

        listHead ->
            listHead :: split (drop chunkSize list) chunkSize
