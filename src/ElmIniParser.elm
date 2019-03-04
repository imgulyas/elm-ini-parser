module ElmIniParser exposing (Ini(..), KeyAndValue(..), Section(..), parseConfigValues, parseIni, parseLineToKV, parseSectionTitle, prepareForIniParsing)

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
    Debug.todo "define this"


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
                        Just <| String.trimLeft chomped
                )
                valueStringParser
    in
    succeed KV
        |= variable
            { start = Char.isAlphaNum
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. spaces
        |. symbol "="
        |= valParser
        |. lineComment ""
        |. oneOf [ symbol "\n", succeed () ]


parseSectionTitle : Parser String
parseSectionTitle =
    let
        myChomper =
            getChompedString <|
                succeed ()
                    |. chompUntil "]"

        titleChomper : Parser String
        titleChomper =
            map
                (\titleWithWhitespace -> String.trimRight titleWithWhitespace)
                myChomper
    in
    succeed identity
        |. spaces
        |. symbol "["
        |. spaces
        |= titleChomper
        |. lineComment "]"


parseConfigValues : Parser ConfigValues
parseConfigValues =
    let
        listParser : Parser (List KeyAndValue)
        listParser =
            loop [] pairsHelp

        pairsHelp : List KeyAndValue -> Parser (Step (List KeyAndValue) (List KeyAndValue))
        pairsHelp pairs =
            oneOf
                [ map
                    (\pair -> Loop (pair :: pairs))
                    parseLineToKV
                , succeed () |> map (\_ -> Done <| List.reverse pairs)
                ]
    in
    map
        (\kvlist ->
            List.map (\(KV key value) -> ( key, value )) kvlist
                |> Dict.fromList
        )
        listParser
