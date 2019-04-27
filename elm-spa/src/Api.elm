module Api exposing (loadSession)

import Http
import Json.Decode exposing (Decoder, andThen, fail, field, int, list, map2, map4, string, succeed)
import Json.Encode as Encode
import Role
import Session
import User



-- SESSION


loadSession : (Result Http.Error User.User -> msg) -> Cmd msg
loadSession msg =
    Http.get
        { url = "/api/session"
        , expect = Http.expectJson msg User.decoder
        }
