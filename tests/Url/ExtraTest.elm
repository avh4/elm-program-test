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
            , check "https://example.com/path?query#fragment" "new" "https://example.com/new"
            , describe "W3 reference examples" <|
                let
                    baseUri =
                        "http://a/b/c/d;p?q"
                in
                [ describe "5.4.1 Normal Examples"
                    [ -- check baseUri "g:h" "g:h"
                      check baseUri "g" "http://a/b/c/g"

                    -- , check baseUri "./g" "http://a/b/c/g"
                    , check baseUri "g/" "http://a/b/c/g/"
                    , check baseUri "/g" "http://a/g"

                    -- , check baseUri "//g" "http://g"
                    -- , check baseUri "?y" "http://a/b/c/d;p?y"
                    , check baseUri "g?y" "http://a/b/c/g?y"

                    -- , check baseUri "#s" "http://a/b/c/d;p?q#s"
                    , check baseUri "g#s" "http://a/b/c/g#s"
                    , check baseUri "g?y#s" "http://a/b/c/g?y#s"
                    , check baseUri ";x" "http://a/b/c/;x"
                    , check baseUri "g;x" "http://a/b/c/g;x"
                    , check baseUri "g;x?y#s" "http://a/b/c/g;x?y#s"

                    -- , check baseUri "" "http://a/b/c/d;p?q"
                    -- , check baseUri "." "http://a/b/c/"
                    -- , check baseUri "./" "http://a/b/c/"
                    -- , check baseUri ".." "http://a/b/"
                    -- , check baseUri "../" "http://a/b/"
                    -- , check baseUri "../g" "http://a/b/g"
                    -- , check baseUri "../.." "http://a/"
                    -- , check baseUri "../../" "http://a/"
                    -- , check baseUri "../../g" "http://a/g"
                    ]
                ]
            ]
        ]
