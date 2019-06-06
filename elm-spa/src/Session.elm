module Session exposing (Session)

import Browser.Navigation as Nav
import Time
import Url
import User


type alias Session =
    { user : Maybe User.User
    , url : Url.Url
    , key : Nav.Key
    , timeZone : Time.Zone
    }
