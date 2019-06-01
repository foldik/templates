module DateTime exposing (toyyyyMMddhhmm)

import Time exposing (..)


toyyyyMMddhhmm : Int -> Time.Zone -> String
toyyyyMMddhhmm posix zone =
    let
        posixTime =
            Time.millisToPosix posix

        year =
            String.fromInt (Time.toYear zone posixTime)

        month =
            toMonthNumberSting (Time.toMonth zone posixTime)

        day =
            String.fromInt (Time.toDay zone posixTime)

        hour =
            String.fromInt (Time.toHour zone posixTime)

        minute =
            String.fromInt (Time.toMinute zone posixTime)
    in
    year ++ "." ++ month ++ "." ++ padLeft day ++ ". " ++ padLeft hour ++ ":" ++ padLeft minute


toMonthNumberSting : Month -> String
toMonthNumberSting month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


padLeft : String -> String
padLeft value =
    String.padLeft 2 '0' value
