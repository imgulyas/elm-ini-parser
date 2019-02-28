module ElmIniParser exposing (Ini(..), Line, Section(..), parse, parseSingleLine, parseToLines)

import Dict exposing (Dict)
import Parser exposing (..)


type Ini
    = Sectionless (Dict String (Maybe String))
    | Sections (List Section)


type Section
    = Section String (Dict String (Maybe String))


type alias Line =
    String


parse : String -> Ini
parse _ =
    Sections []


parseToLines : String -> List Line
parseToLines _ =
    []


parseSingleLine : String -> ( Maybe Line, String )
parseSingleLine _ =
    ( Nothing, "" )
