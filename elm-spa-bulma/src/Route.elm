module Route exposing (Route(..), authorized, router, toLink, toString)

import Model.Role as Role
import Model.Session as Session
import Model.User as User
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | Login
    | Logout
    | Preferences
    | Resources (Maybe Int) (Maybe Int)
    | Resource Int


toLink : Route -> String
toLink route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Login ->
            "/login"

        Logout ->
            "/logout"

        Preferences ->
            "/preferences"

        Resources page size ->
            "/resources" ++ paramsToString [ paramToSring "page" page String.fromInt, paramToSring "limit" size String.fromInt ]

        Resource id ->
            "/resources/" ++ String.fromInt id


paramsToString : List String -> String
paramsToString params =
    let
        parts =
            params
                |> List.filter (\part -> not (String.isEmpty part))
                |> String.join "&"
    in
    if String.isEmpty parts then
        ""

    else
        "?" ++ parts


paramToSring : String -> Maybe a -> (a -> String) -> String
paramToSring name maybeValue convert =
    case maybeValue of
        Just value ->
            name ++ "=" ++ convert value

        Nothing ->
            ""


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "Not found"

        Home ->
            "Home"

        Login ->
            "Login"

        Logout ->
            "Logout"

        Preferences ->
            "Preferences"

        Resources page size ->
            "Resources"

        Resource id ->
            "Resource " ++ String.fromInt id


router : Session.Session -> Route
router session =
    let
        route =
            Maybe.withDefault NotFound (parse routeParser session.url)
    in
    case authorized route session of
        True ->
            route

        False ->
            NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Logout (s "logout")
        , map Preferences (s "preferences")
        , map Resource (s "resources" </> int)
        , map Resources (s "resources" <?> Query.int "page" <?> Query.int "limit")
        ]


authorized : Route -> Session.Session -> Bool
authorized route session =
    let
        authRule =
            authConfig route
    in
    authRule session


authConfig : Route -> (Session.Session -> Bool)
authConfig route =
    case route of
        Home ->
            Session.hasAnyRole [ Role.Admin, Role.SimpleUser ]

        Login ->
            Session.notAuthenticated

        Logout ->
            Session.authenticated

        Preferences ->
            Session.authenticated

        Resources maybePageNumber maybePageSize ->
            Session.hasAnyRole [ Role.Admin ]

        Resource id ->
            Session.hasAnyRole [ Role.Admin ]

        _ ->
            Session.allow
