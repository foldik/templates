module Route exposing (Route(..), authorized, router, toLink, toString)

import Model.Role as Role
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
            "/resources" ++ paramsToString [ paramToSring "page" page String.fromInt, paramToSring "size" size String.fromInt ]


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


router : Url.Url -> Maybe User.User -> Route
router url maybeUser =
    let
        route =
            Maybe.withDefault NotFound (parse routeParser url)
    in
    case authorized route maybeUser of
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
        , map Resources (s "resources" <?> Query.int "page" <?> Query.int "size")
        ]


authorized : Route -> Maybe User.User -> Bool
authorized route maybeUser =
    let
        authPredicate =
            authConfig route
    in
    authPredicate maybeUser


authConfig : Route -> (Maybe User.User -> Bool)
authConfig route =
    case route of
        Home ->
            hasAnyRole [ Role.Admin, Role.SimpleUser ]

        Login ->
            notAuthenticated

        Logout ->
            authenticated

        Preferences ->
            authenticated

        Resources id maybePageNumber ->
            hasAnyRole [ Role.Admin ]

        _ ->
            allow


hasAnyRole : List Role.Role -> (Maybe User.User -> Bool)
hasAnyRole allowedRoles =
    \maybeUser ->
        case maybeUser of
            Nothing ->
                False

            Just user ->
                List.member (User.getRole user) allowedRoles


authenticated : Maybe User.User -> Bool
authenticated maybeUser =
    case maybeUser of
        Just _ ->
            True

        Nothing ->
            False


notAuthenticated : Maybe User.User -> Bool
notAuthenticated maybeUser =
    case maybeUser of
        Just _ ->
            False

        Nothing ->
            True


allow : Maybe User.User -> Bool
allow maybeUser =
    True
