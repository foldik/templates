module Model.Session exposing (Session, allow, authenticated, hasAnyRole, notAuthenticated, setTimeZone, setUrl, setUser)

import Browser.Navigation as Nav
import Model.Role as Role
import Model.User as User
import Time
import Url


type alias Session =
    { maybeUser : Maybe User.User
    , url : Url.Url
    , key : Nav.Key
    , timeZone : Time.Zone
    }


setUser : Session -> Maybe User.User -> Session
setUser session maybeUser =
    { session | maybeUser = maybeUser }


setUrl : Session -> Url.Url -> Session
setUrl session url =
    { session | url = url }


setTimeZone : Session -> Time.Zone -> Session
setTimeZone session timeZone =
    { session | timeZone = timeZone }


hasAnyRole : List Role.Role -> (Session -> Bool)
hasAnyRole allowedRoles =
    \session ->
        case session.maybeUser of
            Nothing ->
                False

            Just user ->
                List.member (User.getRole user) allowedRoles


authenticated : Session -> Bool
authenticated session =
    case session.maybeUser of
        Just _ ->
            True

        Nothing ->
            False


notAuthenticated : Session -> Bool
notAuthenticated session =
    case session.maybeUser of
        Just _ ->
            False

        Nothing ->
            True


allow : Session -> Bool
allow _ =
    True
