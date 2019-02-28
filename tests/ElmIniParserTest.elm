module ElmIniParserTest exposing (takeOutEmptyLinesTest)

import ElmIniParser exposing (..)
import Expect
import Test exposing (Test, describe, test)


takeOutEmptyLinesTest : Test
takeOutEmptyLinesTest =
    describe "takeOutEmptyLines"
        [ describe "should keep original if there is no empty line in it" <|
            [ test "empty string" <|
                \() ->
                    let
                        original =
                            ""
                    in
                    Expect.equal (takeOutEmptyLines original) original
            , test "single line string" <|
                \() ->
                    let
                        original =
                            "asdf"
                    in
                    Expect.equal (takeOutEmptyLines original) original
            , test "multiline string" <|
                \() ->
                    let
                        original =
                            """firstline
                               secondline
                               thirdline"""
                    in
                    Expect.equal (takeOutEmptyLines original) original
            ]
        , describe "should take out empty lines and return the original string without them" <|
            [ test "multiline string" <|
                \() ->
                    let
                        original =
                            """firstline
                               secondline

                               fourthline"""

                        expected =
                            """firstline
                               secondline
                               fourthline"""
                    in
                    Expect.equal (takeOutEmptyLines original) expected
            ]
        ]



-- suite : Test
-- suite =
--     describe "ini parser suite"
--         [ describe "takeOutEmptyLines"
--             [ describe "should keep original if there is no empty line" <|
--                 test "single line string" <|
--                     \() ->
--                         let
--                             original =
--                                 "asdf"
--                         in
--                         Expect.equal (takeOutEmptyLines original) original
--             ]
--         , describe "Line parser"
--             [ test "empty line" <|
--                 \() ->
--                     Expect.equal ( Nothing, "" ) (parseSingleLine "\n")
--             ]
--         ]
