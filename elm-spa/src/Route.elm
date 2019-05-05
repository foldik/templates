module Route exposing (Route(..), router, toString)

import Model.Role as Role
import Model.User as User
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | Page Int (Maybe Int)


router : Url.Url -> Maybe User.User -> Route
router url maybeUser =
    let
        route =
            Maybe.withDefault NotFound (parse routeParser url)
    in
    authorize route maybeUser


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Page (s "page" </> int <?> Query.int "page")
        ]


authorize : Route -> Maybe User.User -> Route
authorize route maybeUser =
    let
        authPredicate =
            authConfig route
    in
    case authPredicate maybeUser of
        True ->
            route

        False ->
            NotFound


authConfig : Route -> (Maybe User.User -> Bool)
authConfig route =
    case route of
        NotFound ->
            allow

        Home ->
            hasAnyRole [ Role.Admin, Role.SimpleUser ]

        Page id maybePageNumber ->
            hasAnyRole [ Role.Admin ]


allow : Maybe User.User -> Bool
allow maybeUser =
    True


hasAnyRole : List Role.Role -> (Maybe User.User -> Bool)
hasAnyRole allowedRoles =
    \maybeUser ->
        case maybeUser of
            Nothing ->
                False

            Just user ->
                List.member (User.getRole user) allowedRoles


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Page id maybePageNumber ->
            case maybePageNumber of
                Just pageNumber ->
                    "/page/" ++ String.fromInt id ++ "?page=" ++ String.fromInt pageNumber

                Nothing ->
                    "/page/" ++ String.fromInt id
