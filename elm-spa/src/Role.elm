module Role exposing (Role(..), decoder)

import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map2, map4, string, succeed)


type Role
    = Admin
    | User


decoder : Decoder Role
decoder =
    let
        convert : String -> Decoder Role
        convert raw =
            if raw == "Admin" then
                succeed Admin

            else if raw == "User" then
                succeed User

            else
                fail "Unknown role"
    in
    string |> andThen convert
