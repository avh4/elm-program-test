module ProgramTest.ComplexQueryTest exposing (all)

import Expect
import Html
import Html.Attributes as HtmlA
import ProgramTest.ComplexQuery as ComplexQuery exposing (Failure(..), FailureContext(..))
import ProgramTest.TestHtmlHacks exposing (FailureReason(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "ProgramTest.ComplexQuery" <|
        let
            html =
                Html.node "body"
                    []
                    [ Html.div []
                        [ Html.form [ HtmlA.id "formA" ]
                            [ Html.button [] []
                            ]
                        ]
                    , Html.text "Outer text"
                    ]
                    |> Query.fromHtml
        in
        [ describe "find"
            [ test "when failing, returns the list of successful selectors" <|
                \() ->
                    ComplexQuery.find
                        [ Selector.tag "form"
                        , Selector.id "formX"
                        ]
                        html
                        |> ComplexQuery.run
                        |> Expect.equal
                            (Err
                                (None
                                    (QueryFailed
                                        (SelectorsFailed
                                            [ Ok "has tag \"form\""
                                            , Err "has attribute \"id\" \"formX\""
                                            ]
                                        )
                                    )
                                )
                            )
            ]
        , describe "errors for composed queries"
            [ test "a find followed by a failing find includes the successful selectors of the first find" <|
                \() ->
                    ComplexQuery.find
                        [ Selector.tag "form"
                        ]
                        html
                        |> ComplexQuery.andThen
                            (ComplexQuery.find
                                [ Selector.tag "button"
                                , Selector.id "buttonX"
                                ]
                            )
                        |> ComplexQuery.run
                        |> Expect.equal
                            (Err
                                (FindSucceeded
                                    [ "has tag \"form\"" ]
                                    (None
                                        (QueryFailed
                                            (SelectorsFailed
                                                [ Ok "has tag \"button\""
                                                , Err "has attribute \"id\" \"buttonX\""
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
            , test "failing after a check should include the check success" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.check
                            "text exists"
                            (ComplexQuery.find [ Selector.text "Outer text" ])
                        |> ComplexQuery.andThen
                            (ComplexQuery.find
                                [ Selector.tag "nope"
                                ]
                            )
                        |> ComplexQuery.run
                        |> Expect.equal
                            (Err
                                (CheckSucceeded "text exists"
                                    (FindSucceeded [ "has text \"Outer text\"" ]
                                        (None ())
                                    )
                                    (None
                                        (QueryFailed
                                            (SelectorsFailed
                                                [ Err "has tag \"nope\""
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
            ]
        ]
