module ProgramTest.StringLinesTest exposing (all)

import Expect
import ProgramTest.StringLines as StringLines
import Test exposing (..)


all : Test
all =
    describe "ProgramTest.StringLines"
        [ describe "charAt"
            [ test "first char" <|
                \() ->
                    StringLines.charAt 1 1 "abc"
                        |> Expect.equal (Just "a")
            , test "first line" <|
                \() ->
                    StringLines.charAt 1 3 "abcd"
                        |> Expect.equal (Just "c")
            , test "later line" <|
                \() ->
                    StringLines.charAt 2 3 "abcd\nABCD"
                        |> Expect.equal (Just "C")
            ]
        , describe "replaceAt"
            [ test "first char" <|
                \() ->
                    StringLines.replaceAt 1 1 "**" "abc"
                        |> Expect.equal "**bc"
            , test "first line" <|
                \() ->
                    StringLines.replaceAt 1 3 "**" "abcd"
                        |> Expect.equal "ab**d"
            , test "later line" <|
                \() ->
                    StringLines.replaceAt 2 3 "**" "abcd\nABCD"
                        |> Expect.equal "abcd\nAB**D"
            ]
        ]
