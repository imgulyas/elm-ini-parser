module ElmIniParserTest exposing (parseLineToKVTest, prepareForIniParsingTest)

import Debug
import ElmIniParser exposing (..)
import Expect
import List
import Parser
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
                            , "second; this is a line ending comment"
                            ]

                    expected =
                        join
                            [ "first"
                            , "second"
                            ]
                in
                Expect.equal (prepareForIniParsing original) expected
        ]


interpolate : List String -> String -> String
interpolate interp s =
    let
        split =
            String.split "{}" s

        zipped =
            List.map2
                (\spl -> \int -> int ++ spl)
                split
                ("" :: interp)

        interpolated =
            String.concat zipped
    in
    interpolated


parseLineToKVTest : Test
parseLineToKVTest =
    describe "parse prepared line to key and value"
        [ test "with some value until end of string" <|
            \() ->
                let
                    key =
                        "mykey"

                    value =
                        "myvalue"

                    testLine =
                        prepareForIniParsing <| interpolate [ key, value ] "  {} = {}"

                    result =
                        Parser.run parseLineToKV testLine

                    expected =
                        Ok (KV key (Just value))
                in
                Expect.equal expected result
        , test "with some value until end of line" <|
            \() ->
                let
                    key =
                        "mykey"

                    value =
                        "myvalue"

                    testLine =
                        prepareForIniParsing <| interpolate [ key, value ] "  {} = {}  \n"

                    result =
                        Parser.run parseLineToKV testLine

                    expected =
                        Ok (KV key (Just value))
                in
                Expect.equal expected result
        , test "with missing value" <|
            \() ->
                let
                    key =
                        "mykey"

                    testLine =
                        prepareForIniParsing <| interpolate [ key ] "  {} =  \n"

                    result =
                        Parser.run parseLineToKV testLine

                    expected =
                        Ok (KV key (Nothing))
                in
                Expect.equal expected result
        ]
