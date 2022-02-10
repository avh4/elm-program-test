module ProgramTest.ComplexQueryTest exposing (all)

import Expect
import Html
import Html.Attributes as HtmlA
import ProgramTest.ComplexQuery as ComplexQuery exposing (Failure(..))
import Set
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
                    >> Tuple.mapBoth
                        Set.toList
                        (Result.mapError (Tuple.mapFirst (List.map forceFailureContext)))
        in
        [ describe "find"
            [ test "when failing, returns the list of successful selectors" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.find Nothing
                            [ "form" ]
                            [ Selector.tag "form"
                            , Selector.id "formX"
                            ]
                        |> run
                        |> Expect.equal
                            ( [ "form" ]
                            , Err
                                ( []
                                , QueryFailed
                                    [ Ok "has tag \"form\""
                                    , Err "has attribute \"id\" \"formX\""
                                    ]
                                )
                            )
            ]
        , describe "findButNot"
            [ test "error report when bad check succeeds" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.findButNot (Just "expected bad")
                            [ "form" ]
                            { good = [ Selector.tag "form" ]
                            , bads = [ [ Selector.id "formA" ] ]
                            , onError = [ Selector.tag "form", Selector.id "form<notA>" ]
                            }
                        |> run
                        |> Expect.equal
                            ( [ "form" ]
                            , Err
                                ( [ Description (Err "expected bad") ]
                                , QueryFailed
                                    [ Ok "has tag \"form\""
                                    , Err "has attribute \"id\" \"form<notA>\""
                                    ]
                                )
                            )
            ]
        , describe "errors for composed queries"
            [ test "a find followed by a failing find includes the successful selectors of the first find" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.find Nothing
                            [ "form" ]
                            [ Selector.tag "form"
                            ]
                        |> ComplexQuery.find (Just "expected to fail")
                            [ "button" ]
                            [ Selector.tag "button"
                            , Selector.id "buttonX"
                            ]
                        |> run
                        |> Expect.equal
                            ( [ "button", "form" ]
                            , Err
                                ( [ FindSucceeded Nothing [ "has tag \"form\"" ]
                                  , Description (Err "expected to fail")
                                  ]
                                , QueryFailed
                                    [ Ok "has tag \"button\""
                                    , Err "has attribute \"id\" \"buttonX\""
                                    ]
                                )
                            )
            , test "failing after a check should include the check success" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.check
                            "text exists"
                            (ComplexQuery.find Nothing [] [ Selector.text "Outer text" ])
                        |> ComplexQuery.find (Just "expected to fail")
                            []
                            [ Selector.tag "nope" ]
                        |> run
                        |> Expect.equal
                            ( []
                            , Err
                                ( [ CheckSucceeded "text exists"
                                        [ FindSucceeded Nothing
                                            [ "has text \"Outer text\"" ]
                                        ]
                                  , Description (Err "expected to fail")
                                  ]
                                , QueryFailed
                                    [ Err "has tag \"nope\""
                                    ]
                                )
                            )
            , test "multiple checks should not duplicate context information" <|
                \() ->
                    ComplexQuery.succeed html
                        |> ComplexQuery.check
                            "text exists"
                            (ComplexQuery.find Nothing [] [ Selector.text "Outer text" ])
                        |> ComplexQuery.check "button exists"
                            (ComplexQuery.find Nothing [] [ Selector.tag "button" ])
                        |> ComplexQuery.find (Just "expected to fail")
                            []
                            [ Selector.tag "nope" ]
                        |> run
                        |> Expect.equal
                            ( []
                            , Err
                                ( [ CheckSucceeded "text exists"
                                        [ FindSucceeded Nothing
                                            [ "has text \"Outer text\"" ]
                                        ]
                                  , CheckSucceeded "button exists"
                                        [ FindSucceeded Nothing
                                            [ "has tag \"button\"" ]
                                        ]
                                  , Description (Err "expected to fail")
                                  ]
                                , QueryFailed
                                    [ Err "has tag \"nope\""
                                    ]
                                )
                            )
            ]
        ]


type ForcedFailureContext
    = FindSucceeded (Maybe String) (List String)
    | CheckSucceeded String (List ForcedFailureContext)
    | Description (Result String String)


forceFailureContext : ComplexQuery.FailureContext1 -> ForcedFailureContext
forceFailureContext failureContext =
    case failureContext of
        ComplexQuery.FindSucceeded desc selectors ->
            FindSucceeded desc (selectors ())

        ComplexQuery.CheckSucceeded desc check ->
            CheckSucceeded desc (List.map forceFailureContext check)

        ComplexQuery.Description desc ->
            Description desc
