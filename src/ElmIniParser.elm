module ElmIniParser exposing (Ini(..), KeyAndValue(..), Section(..), parseIni, parseLineToKV, prepareForIniParsing)

import Dict exposing (Dict)
import Parser exposing (..)
import Set
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
    succeed Ini
        |. symbol "["
        |. spaces
        |= chompUntil "]"
        |. end



--    Debug.todo "define this"


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


joinIniLineBreaks : String -> String
joinIniLineBreaks =
    S.replace "\\\n" ""


removeLineEndingComments : String -> String
removeLineEndingComments =
    S.lines
        >> List.map
            (S.split ";"
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


type KeyAndValue
    = KV String (Maybe String)


parseLineToKV : Parser KeyAndValue
parseLineToKV =
    let
        valueStringParser : Parser String
        valueStringParser =
            getChompedString <|
                succeed ()
                    |. chompUntilEndOr "\n"

        valParser : Parser (Maybe String)
        valParser =
            map
                (\chomped ->
                    if S.isEmpty chomped then
                        Nothing

                    else
                        Just chomped
                )
                valueStringParser
    in
    succeed KV
        |. spaces
        |= variable
            { start = Char.isAlphaNum
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. spaces
        |. symbol "="
        |. spaces
        |= valParser
        |. lineComment ""
