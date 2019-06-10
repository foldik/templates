module Route exposing (Route(..), router, toLink, toString)

import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | NewProject
    | NewUser
    | Services


toLink : Route -> String
toLink route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        NewProject ->
            "/projects/new"

        NewUser ->
            "/users/new"

        Services ->
            "/services"


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "Not found"

        Home ->
            "Home"

        NewProject ->
            "New project"

        NewUser ->
            "New user"

        Services ->
            "Services"


router : Url.Url -> Route
router url =
    Maybe.withDefault NotFound (parse routeParser url)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map NewProject (s "projects" </> s "new")
        , map NewUser (s "users" </> s "new")
        , map Services (s "services")
        ]
