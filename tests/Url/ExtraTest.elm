module Url.ExtraTest exposing (all)

import Browser exposing (UrlRequest(..))
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
                                    Just (Url.Extra.resolve b relative)
                                        |> Expect.equal (Url.fromString expected)
            in
            [ check "https://example.com/path" "https://example.com/new" "https://example.com/new"
            , check "https://example.com/path" "/new" "https://example.com/new"
            , check "https://example.com/path" "new" "https://example.com/new"
            , check "https://example.com/path/file" "new" "https://example.com/path/new"
            , check "https://example.com/path?query#fragment" "new" "https://example.com/new"
            , check "https://example.com/path?query#fragment" "new?q#f" "https://example.com/new?q#f"
            , check "https://example.com/path?query#fragment" "?q#f" "https://example.com/path?q#f"
            , check "https://example.com/path?query#fragment" "#f" "https://example.com/path?query#f"
            , check "https://example.com/path?query#fragment" "?q" "https://example.com/path?q"
            , check "https://example.com/path?query#fragment" "" "https://example.com/path?query#fragment"
            , check "https://example.com/path?query#fragment" "../path2" "https://example.com/path2"
            , check "https://example.com/path" "/new?q#f" "https://example.com/new?q#f"
            , check "https://example.com/path" "/new?q" "https://example.com/new?q"
            , check "https://example.com/path" "/new#f" "https://example.com/new#f"
            , check "https://example.com/path" "//new.example.com?q#f" "https://new.example.com?q#f"
            , check "https://example.com/path" "//new.example.com/new?q#f" "https://new.example.com/new?q#f"
            , describe "W3 reference examples" <|
                let
                    baseUri =
                        "http://a/b/c/d;p?q"
                in
                [ describe "5.4.1 Normal Examples"
                    [ -- check baseUri "g:h" "g:h"
                      check baseUri "g" "http://a/b/c/g"
                    , check baseUri "./g" "http://a/b/c/g"
                    , check baseUri "g/" "http://a/b/c/g/"
                    , check baseUri "/g" "http://a/g"
                    , check baseUri "//g" "http://g/" -- In 5.4.1 Normal Examples this is "http://g", but Url.Parser defaults to "/" for empty path segment
                    , check baseUri "?y" "http://a/b/c/d;p?y"
                    , check baseUri "g?y" "http://a/b/c/g?y"
                    , check baseUri "#s" "http://a/b/c/d;p?q#s"
                    , check baseUri "g#s" "http://a/b/c/g#s"
                    , check baseUri "g?y#s" "http://a/b/c/g?y#s"
                    , check baseUri ";x" "http://a/b/c/;x"
                    , check baseUri "g;x" "http://a/b/c/g;x"
                    , check baseUri "g;x?y#s" "http://a/b/c/g;x?y#s"
                    , check baseUri "" "http://a/b/c/d;p?q"
                    , check baseUri "." "http://a/b/c/"
                    , check baseUri "./" "http://a/b/c/"
                    , check baseUri ".." "http://a/b/"
                    , check baseUri "../" "http://a/b/"
                    , check baseUri "../g" "http://a/b/g"
                    , check baseUri "../.." "http://a/"
                    , check baseUri "../../" "http://a/"
                    , check baseUri "../../g" "http://a/g"
                    ]
                ]
            ]
        , describe "toUrlRequest" <|
            let
                requestToString req =
                    case req of
                        Internal url ->
                            "INTERNAL:" ++ Url.toString url

                        External href ->
                            "EXTERNAL:" ++ href

                check base relative expected =
                    test ("resolve " ++ base ++ " " ++ relative) <|
                        \() ->
                            case Url.fromString base of
                                Nothing ->
                                    Expect.fail ("Unable to parse base url: " ++ base)

                                Just b ->
                                    Url.Extra.toUrlRequest b relative
                                        |> requestToString
                                        |> Expect.equal expected
            in
            [ check "https://example.com/path" "https://example.com/new" "INTERNAL:https://example.com/new"
            , check "https://example.com/path" "/new" "INTERNAL:https://example.com/new"
            , check "https://example.com/path" "new" "INTERNAL:https://example.com/new"
            , check "https://example.com/path/file" "new?query#fragment" "INTERNAL:https://example.com/path/new?query#fragment"
            , check "https://example.com/path?query#fragment" "new" "INTERNAL:https://example.com/new"
            , check "http://localhost:3000" "http://localhost:3000/new" "INTERNAL:http://localhost:3000/new"
            , check "https://example.com/path/file" "https://example2.com/new" "EXTERNAL:https://example2.com/new"
            , check "https://example.com/path/file" "http://example.com/path/file" "EXTERNAL:http://example.com/path/file"
            , check "http://localhost:3000" "http://localhost:5000" "EXTERNAL:http://localhost:5000"
            ]
        ]
