module Session exposing (Session(..), getNavKey, getUrl, getZone, guest, isSignedIn, setUrl, setZone, toNotSignedIn, toSignedIn)

import Browser.Navigation as Nav
import Time
import Url
import User


type Session
    = SignedIn Time.Zone Nav.Key Url.Url User.User
    | NotSignedIn Time.Zone Nav.Key Url.Url


isSignedIn : Session -> Bool
isSignedIn session =
    case session of
        SignedIn _ _ _ _ ->
            True

        NotSignedIn _ _ _ ->
            False


toSignedIn : Session -> User.User -> Session
toSignedIn session user =
    case session of
        NotSignedIn zone key url ->
            SignedIn zone key url user

        SignedIn zone key url _ ->
            SignedIn zone key url user


toNotSignedIn : Session -> Session
toNotSignedIn session =
    case session of
        NotSignedIn zone key url ->
            NotSignedIn zone key url

        SignedIn zone key url _ ->
            NotSignedIn zone key url


guest : Nav.Key -> Url.Url -> Session
guest key url =
    NotSignedIn Time.utc key url


getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        NotSignedIn _ key _ ->
            key

        SignedIn _ key _ _ ->
            key


getUrl : Session -> Url.Url
getUrl session =
    case session of
        NotSignedIn _ _ url ->
            url

        SignedIn _ _ url _ ->
            url


setUrl : Session -> Url.Url -> Session
setUrl session url =
    case session of
        NotSignedIn zone key _ ->
            NotSignedIn zone key url

        SignedIn zone key _ user ->
            SignedIn zone key url user


getZone : Session -> Time.Zone
getZone session =
    case session of
        NotSignedIn zone _ _ ->
            zone

        SignedIn zone _ _ _ ->
            zone


setZone : Session -> Time.Zone -> Session
setZone session zone =
    case session of
        NotSignedIn _ key url ->
            NotSignedIn zone key url

        SignedIn _ key url user ->
            SignedIn zone key url user
