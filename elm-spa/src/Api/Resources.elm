module Api.Resources exposing (NewResource, PaginatedList, Resource, createResource, getResource, getResources)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map, map2, map4, string, succeed)
import Json.Encode as Encode



-- MODEL


type alias NewResource =
    { name : String
    }


encodeNewResource : NewResource -> Encode.Value
encodeNewResource newResource =
    Encode.object
        [ ( "name", Encode.string newResource.name ) ]


type alias Resource =
    { id : Int
    , name : String
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
    map2 Resource
        (field "id" int)
        (field "name" string)



-- API


createResource : (Result Http.Error Resource -> msg) -> NewResource -> Cmd msg
createResource msg newResource =
    Http.post
        { url = "/api/resources"
        , body = Http.jsonBody (encodeNewResource newResource)
        , expect = Http.expectJson msg resourceDecoder
        }


getResources : (Result Http.Error (PaginatedList Resource) -> msg) -> Cmd msg
getResources msg =
    Http.get
        { url = "/api/resources?page=1&limit=20"
        , expect = Http.expectJson msg (paginatedListDecoder resourceDecoder)
        }


getResource : Int -> (Result Http.Error Resource -> msg) -> Cmd msg
getResource id msg =
    Http.get
        { url = "/api/resources/" ++ String.fromInt id
        , expect = Http.expectJson msg resourceDecoder
        }
