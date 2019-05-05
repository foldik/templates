module Route exposing (Route(..), authorized, router, toLink, toString)

import Model.Role as Role
import Model.User as User
import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | Resources Int (Maybe Int)


toLink : Route -> String
toLink route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Resources id maybePageNumber ->
            case maybePageNumber of
                Just pageNumber ->
                    "/resources/" ++ String.fromInt id ++ "?page=" ++ String.fromInt pageNumber

                Nothing ->
                    "/resources/" ++ String.fromInt id


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "Not found"

        Home ->
            "Home"

        Resources id maybePageNumber ->
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
        , map Resources (s "resources" </> int <?> Query.int "page")
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

        Resources id maybePageNumber ->
            hasAnyRole [ Role.Admin ]

        _ ->
            allow


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
