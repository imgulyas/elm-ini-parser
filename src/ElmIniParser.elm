module ElmIniParser exposing (Ini(..), Section(..), parseIni, prepareForIniParsing)

import Dict exposing (Dict)
import Parser exposing (..)
import String as S


type Ini
    = WithGlobals ConfigValues (List Section)
    | WithoutGlobals (List Section)


type Section
    = Section String ConfigValues


type alias ConfigValues =
    Dict String (Maybe String)


parseIni : String -> Ini
parseIni _ =
    Debug.todo "define this"


joinIniLineBreaks : String -> String
joinIniLineBreaks =
    S.replace "\\\n" ""


removeLineEndingComments : String -> String
removeLineEndingComments =
    S.lines
        >> List.map
            (S.split "#"
                >> (\splitstr ->
                        case splitstr of
                            [] ->
                                ""

                            h :: tail ->
                                h
                   )
            )
        >> S.join "\n"


removeFullLineComments : String -> String
removeFullLineComments =
    S.lines
        >> List.filter (S.startsWith ";" >> not)
        >> S.join "\n"


removeEmptyLines : String -> String
removeEmptyLines =
    S.lines
        >> List.filter (S.isEmpty >> not)
        >> S.join "\n"


trimWhitespace : String -> String
trimWhitespace =
    S.lines
        >> List.map S.trim
        >> S.join "\n"


prepareForIniParsing : String -> String
prepareForIniParsing =
    removeLineEndingComments
        >> trimWhitespace
        >> joinIniLineBreaks
        >> removeEmptyLines
        >> removeFullLineComments
