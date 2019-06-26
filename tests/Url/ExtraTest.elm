module Url.ExtraTest exposing (all)

import Expect
import Test exposing (..)
import Url
import Url.Extra


all : Test
all =
    describe "Url.Extra"
        [ describe "resolve" <|
            let
                check base relative expected =
                    test ("resolve " ++ base ++ " " ++ relative) <|
                        \() ->
                            case Url.fromString base of
                                Nothing ->
                                    Expect.fail ("Unable to parse base url: " ++ base)

                                Just b ->
                                    Url.Extra.resolve b relative
                                        |> Url.toString
                                        |> Expect.equal expected
            in
            [ check "https://example.com/path" "https://example.com/new" "https://example.com/new"
            , check "https://example.com/path" "/new" "https://example.com/new"
            , check "https://example.com/path" "new" "https://example.com/new"
            , check "https://example.com/path/file" "new" "https://example.com/path/new"
            ]
        ]
