module ElmIniParser exposing (Ini(..), Section(..), parseIni, takeOutEmptyLines)

import Dict exposing (Dict)
import Parser exposing (..)


type Ini
    = WithGlobals ConfigValues (List Section)
    | WithoutGlobals (List Section)


type Section
    = Section String ConfigValues


type alias ConfigValues =
    Dict String (Maybe String)



-- a prepared line
--   is not empty
--   does not contain comments


type PreparedLine
    = PL String


parseIni : String -> Ini
parseIni _ =
    WithoutGlobals []


takeOutEmptyLines : String -> String
takeOutEmptyLines s =
    s
        |> String.lines
        |> List.concatMap
            (\line ->
                case line of
                    "" ->
                        []

                    other ->
                        [ other ]
            )
        |> String.join "\n"


parseToPreparedLines : String -> List PreparedLine
parseToPreparedLines _ =
    []


parseSingleLine : String -> ( Maybe PreparedLine, String )
parseSingleLine _ =
    ( Nothing, "" )
