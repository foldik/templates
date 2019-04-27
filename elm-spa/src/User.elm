module User exposing (User, decoder)

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map2, map5, string, succeed)
import Role


type alias User =
    { id : Int
    , userName : String
    , firstName : String
    , lastName : String
    , role : Role.Role
    }


decoder : Decoder User
decoder =
    map5 User
        (field "id" int)
        (field "username" string)
        (field "first_name" string)
        (field "last_name" string)
        (field "role" Role.decoder)
