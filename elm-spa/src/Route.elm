module Route exposing (Route(..), router, toString)

import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = NotFound
    | Home
    | Page Int


router : Url.Url -> Route
router url =
    Maybe.withDefault NotFound (parse routeParser url)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Page (s "page" </> int)
        ]


toString : Route -> String
toString route =
    case route of
        NotFound ->
            "/not-found"

        Home ->
            "/"

        Page id ->
            "/page/" ++ String.fromInt id
