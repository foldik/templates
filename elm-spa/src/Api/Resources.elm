module Api.Resources exposing (NewResource, PaginatedList, Resource, createResource, getResource, getResources)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map, map2, map3, map4, string, succeed)
import Json.Encode as Encode



-- MODEL


type alias NewResource =
    { name : String
    , shortDescription : String
    }


encodeNewResource : NewResource -> Encode.Value
encodeNewResource newResource =
    Encode.object
        [ ( "name", Encode.string newResource.name )
        , ( "short_description", Encode.string newResource.shortDescription )
        ]


type alias Resource =
    { id : Int
    , name : String
    , shortDescription : String
    }


type alias PaginatedList a =
    { page : Int
    , limit : Int
    , count : Int
    , data : List a
    }


paginatedListDecoder : Decoder a -> Decoder (PaginatedList a)
paginatedListDecoder listDecoder =
    map4 PaginatedList
        (field "page" int)
        (field "limit" int)
        (field "count" int)
        (field "data" (list listDecoder))


resourceDecoder : Decoder Resource
resourceDecoder =
    map3 Resource
        (field "id" int)
        (field "name" string)
        (field "short_description" string)



-- API


createResource : (Result Http.Error Resource -> msg) -> NewResource -> Cmd msg
createResource msg newResource =
    Http.post
        { url = "/api/resources"
        , body = Http.jsonBody (encodeNewResource newResource)
        , expect = Http.expectJson msg resourceDecoder
        }


getResources : Int -> Int -> (Result Http.Error (PaginatedList Resource) -> msg) -> Cmd msg
getResources page limit msg =
    Http.get
        { url = "/api/resources?page=" ++ String.fromInt page ++ "&limit=" ++ String.fromInt limit
        , expect = Http.expectJson msg (paginatedListDecoder resourceDecoder)
        }


getResource : Int -> (Result Http.Error Resource -> msg) -> Cmd msg
getResource id msg =
    Http.get
        { url = "/api/resources/" ++ String.fromInt id
        , expect = Http.expectJson msg resourceDecoder
        }
