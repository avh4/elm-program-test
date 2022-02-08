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
        ]
