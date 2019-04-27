module Route exposing (Route(..), router)

import Url
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | NotFound
    | ReloadSession


router : Url.Url -> Route
router url =
    Maybe.withDefault NotFound (parse routeParser url)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map ReloadSession (s "reload-session")
        ]
