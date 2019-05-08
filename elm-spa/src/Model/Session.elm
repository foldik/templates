module Model.Session exposing (Session, setUrl, setUser)

import Browser.Navigation as Nav
import Model.Role as Role
import Model.User as User
import Url


type alias Session =
    { maybeUser : Maybe User.User
    , url : Url.Url
    , key : Nav.Key
    }


setUser : Session -> Maybe User.User -> Session
setUser session maybeUser =
    { session | maybeUser = maybeUser }


setUrl : Session -> Url.Url -> Session
setUrl session url =
    { session | url = url }
