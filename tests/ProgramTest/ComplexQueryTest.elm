module ProgramTest.ComplexQueryTest exposing (all)

import Expect
import Html
import Html.Attributes as HtmlA
import ProgramTest.ComplexQuery as ComplexQuery exposing (Failure(..))
import ProgramTest.TestHtmlHacks exposing (FailureReason(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "ProgramTest.ComplexQuery" <|
        let
            html =
                Html.div []
                    [ Html.form [ HtmlA.id "formA" ]
                        []
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
                                (QueryFailed
                                    (SelectorsFailed
                                        [ Ok "has tag \"form\""
                                        , Err "has attribute \"id\" \"formX\""
                                        ]
                                    )
                                )
                            )
            ]
        ]
