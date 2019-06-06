module Route exposing (Route(..), router, toLink, toString)

import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | Form


toLink : Route -> String
toLink route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Form ->
            "/form"


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "Not found"

        Home ->
            "Home"

        Form ->
            "Form"


router : Url.Url -> Route
router url =
    Maybe.withDefault NotFound (parse routeParser url)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Form (s "form")
        ]
