module ProgramTest.HtmlHighlighterTest exposing (all)

import Expect
import Html.Parser
import ProgramTest.HtmlHighlighter as HtmlHighlighter exposing (Node(..))
import Test exposing (..)


all : Test
all =
    describe "ProgramTest.HtmlHighlighter"
        [ test "hides elements that don't match" <|
            \() ->
                Html.Parser.Element "div"
                    []
                    [ Html.Parser.Element "span" [] []
                    , Html.Parser.Element "button" [] []
                    ]
                    |> HtmlHighlighter.highlight (\tag _ _ -> tag == "button")
                    |> Expect.equal
                        (Element "div"
                            []
                            [ Hidden "<span>...</span>"
                            , Element "button" [] []
                            ]
                        )
        , describe "hidden element descriptions"
            [ test "prefers id" <|
                \() ->
                    Html.Parser.Element "div"
                        [ ( "class", "C" )
                        , ( "id", "I" )
                        , ( "name", "N" )
                        ]
                        []
                        |> HtmlHighlighter.highlight (\_ _ _ -> False)
                        |> Expect.equal
                            (Hidden "<div id=\"I\">...</div>")
            , test "then prefers name" <|
                \() ->
                    Html.Parser.Element "input"
                        [ ( "name", "N" )
                        , ( "class", "C" )
                        ]
                        []
                        |> HtmlHighlighter.highlight (\_ _ _ -> False)
                        |> Expect.equal
                            (Hidden "<input name=\"N\">...</input>")
            , test "then prefers class" <|
                \() ->
                    Html.Parser.Element "div"
                        [ ( "class", "C" )
                        ]
                        []
                        |> HtmlHighlighter.highlight (\_ _ _ -> False)
                        |> Expect.equal
                            (Hidden "<div class=\"C\">...</div>")
            ]
        ]
