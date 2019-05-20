module Api.Resources exposing (NewResource, Resource, createResource, getResource, getResources)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map2, map4, string, succeed)
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


getResources : (Result Http.Error (List Resource) -> msg) -> Cmd msg
getResources msg =
    Http.get
        { url = "/api/resources"
        , expect = Http.expectJson msg (list resourceDecoder)
        }


getResource : Int -> (Result Http.Error Resource -> msg) -> Cmd msg
getResource id msg =
    Http.get
        { url = "/api/resources/" ++ String.fromInt id
        , expect = Http.expectJson msg resourceDecoder
        }
