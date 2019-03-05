module Ini exposing (parseIni)

import Dict
import ElmIniParser
import Parser


parseIni : String -> Result String ElmIniParser.Ini
parseIni text =
    Debug.log "prepared text" (ElmIniParser.prepareForIniParsing text)
        |> Parser.run ElmIniParser.ini
        |> Result.mapError (\error -> Debug.toString error)
