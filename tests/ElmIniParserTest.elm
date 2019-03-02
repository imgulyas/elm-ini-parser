module ElmIniParserTest exposing (prepareForIniParsingTest)

import ElmIniParser exposing (..)
import Expect
import Test exposing (Test, describe, test)


join : List String -> String
join =
    String.join "\n"


prepareForIniParsingTest : Test
prepareForIniParsingTest =
    describe "prepareForIniParsing"
        [ test "should join lines that have INI line breaks between them" <|
            \() ->
                let
                    original =
                        join
                            [ "first"
                            , "second\\"
                            , "third"
                            , "fourth\\"
                            , "fifth\\"
                            , "sixth"
                            ]

                    expected =
                        join
                            [ "first"
                            , "secondthird"
                            , "fourthfifthsixth"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        , test "should filter out empty lines" <|
            \() ->
                let
                    original =
                        join
                            [ "first"
                            , "second"
                            , ""
                            , "fourth"
                            ]

                    expected =
                        join
                            [ "first"
                            , "second"
                            , "fourth"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        , test "should filter out full line comments" <|
            \() ->
                let
                    original =
                        join
                            [ "first"
                            , ";this is a line comment"
                            , ";this too"
                            ]

                    expected =
                        join
                            [ "first"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        , test "should trim whitespace in front and end of lines" <|
            \() ->
                let
                    original =
                        join
                            [ "    first   \t"
                            , "      second"
                            , " third"
                            ]

                    expected =
                        join
                            [ "first"
                            , "second"
                            , "third"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        , test "should remove line-ending comments" <|
            \() ->
                let
                    original =
                        join
                            [ "first"
                            , "second# this is a line ending comment"
                            ]

                    expected =
                        join
                            [ "first"
                            , "second"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        ]
