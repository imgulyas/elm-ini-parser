module ParserTest exposing (suite)

import ElmIniParser exposing (..)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Line parser"
        [ test "empty line" <|
            \() ->
                Expect.equal ( Nothing, "" ) (parseSingleLine "\n")
        ]
