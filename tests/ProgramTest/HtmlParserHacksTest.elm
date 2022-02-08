module ProgramTest.HtmlParserHacksTest exposing (all)

import Expect
import Html.Parser exposing (Node(..))
import ProgramTest.HtmlParserHacks as HtmlParserHacks
import Test exposing (..)


all : Test
all =
    describe "ProgramTest.HtmlParserHacks"
        [ test "can parse HTML" <|
            \() ->
                """<div class="container"><span>Hi</span>Bye</div>"""
                    |> HtmlParserHacks.parse
                    |> Expect.equal
                        (Ok
                            [ Element "div"
                                [ ( "class", "container" ) ]
                                [ Element "span" [] [ Text "Hi" ]
                                , Text "Bye"
                                ]
                            ]
                        )
        , test "can parse HTML with tag-like text content" <|
            \() ->
                """<div><INIT></div>"""
                    |> HtmlParserHacks.parse
                    |> Expect.equal
                        (Ok
                            [ Element "div" [] [ Text "<INIT>" ]
                            ]
                        )

        --, test "can parse HTML with tag-like text content (nested)" <|
        --    \() ->
        --        """<div><span><INIT></span></div>"""
        --            |> HtmlParserHacks.parse
        --            |> Expect.equal
        --                (Ok
        --                    [ Element "div"
        --                        []
        --                        [ Element "span"
        --                            []
        --                            [ Text "<INIT>"
        --                            ]
        --                        ]
        --                    ]
        --                )
        ]
