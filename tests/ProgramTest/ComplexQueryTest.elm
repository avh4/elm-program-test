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

            run =
                ComplexQuery.run
                    >> Result.mapError (Tuple.mapSecond forceFailureContext)
        in
        [ describe "find"
            [ test "when failing, returns the list of successful selectors" <|
                \() ->
                    ComplexQuery.find Nothing
                        [ "form" ]
                        [ Selector.tag "form"
                        , Selector.id "formX"
                        ]
                        html
                        |> run
                        |> Expect.equal
                            (Err
                                ( [ "form" ]
                                , None
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
        , describe "findButNot"
            [ test "error report when bad check succeeds" <|
                \() ->
                    ComplexQuery.findButNot (Just "expected bad")
                        [ "form" ]
                        { good = [ Selector.tag "form" ]
                        , bads = [ [ Selector.id "formA" ] ]
                        , onError = [ Selector.tag "form", Selector.id "form<notA>" ]
                        }
                        html
                        |> run
                        |> Expect.equal
                            (Err
                                ( [ "form" ]
                                , Description (Err "expected bad")
                                    (None
                                        (QueryFailed
                                            (SelectorsFailed
                                                [ Ok "has tag \"form\""
                                                , Err "has attribute \"id\" \"form<notA>\""
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
            ]
        , describe "errors for composed queries"
            [ test "a find followed by a failing find includes the successful selectors of the first find" <|
                \() ->
                    ComplexQuery.find Nothing
                        [ "form" ]
                        [ Selector.tag "form"
                        ]
                        html
                        |> ComplexQuery.andThen
                            (ComplexQuery.find (Just "expected to fail")
                                [ "button" ]
                                [ Selector.tag "button"
                                , Selector.id "buttonX"
                                ]
                            )
                        |> run
                        |> Expect.equal
                            (Err
                                ( [ "form", "button" ]
                                , FindSucceeded
                                    Nothing
                                    [ "has tag \"form\"" ]
                                    (Description (Err "expected to fail")
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
                            )
            , test "failing after a check should include the check success" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.check
                            "text exists"
                            (ComplexQuery.find Nothing [] [ Selector.text "Outer text" ])
                        |> ComplexQuery.andThen
                            (ComplexQuery.find (Just "expected to fail")
                                []
                                [ Selector.tag "nope"
                                ]
                            )
                        |> run
                        |> Expect.equal
                            (Err
                                ( []
                                , CheckSucceeded "text exists"
                                    (FindSucceeded Nothing
                                        [ "has text \"Outer text\"" ]
                                        (None ())
                                    )
                                    (Description (Err "expected to fail")
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
                            )
            ]
        ]


type ForcedFailureContext a
    = FindSucceeded (Maybe String) (List String) (ForcedFailureContext a)
    | CheckSucceeded String (ForcedFailureContext ()) (ForcedFailureContext a)
    | Description (Result String String) (ForcedFailureContext a)
    | None a


forceFailureContext : ComplexQuery.FailureContext a -> ForcedFailureContext a
forceFailureContext failureContext =
    case failureContext of
        ComplexQuery.FindSucceeded desc selectors next ->
            FindSucceeded desc (selectors ()) (forceFailureContext next)

        ComplexQuery.CheckSucceeded desc check next ->
            CheckSucceeded desc (forceFailureContext_ check) (forceFailureContext next)

        ComplexQuery.Description desc next ->
            Description desc (forceFailureContext next)

        ComplexQuery.None a ->
            None a


forceFailureContext_ : ComplexQuery.FailureContext a -> ForcedFailureContext a
forceFailureContext_ =
    forceFailureContext
