module TestContextHttpTests exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Events exposing (onClick)
import Test exposing (..)
import Test.Runner
import TestContext exposing (TestContext)


type TestEffect
    = NoEffect
    | HttpGet String


deconstructEffect : TestEffect -> List TestContext.SimulatedEffect
deconstructEffect testEffect =
    case testEffect of
        NoEffect ->
            []

        HttpGet url ->
            [ TestContext.HttpRequest { method = "GET", url = url } ]


type TestMsg
    = PassThroughEffect TestEffect


start : TestEffect -> TestContext TestMsg () TestEffect
start initialEffect =
    TestContext.createWithSimulatedEffects
        { init = ( (), initialEffect )
        , update =
            \msg () ->
                case msg of
                    PassThroughEffect effect ->
                        ( (), effect )
        , view =
            \() ->
                Html.div []
                    [ Html.button
                        [ onClick (PassThroughEffect (HttpGet "https://example.com/buttons/get")) ]
                        [ Html.text "Get" ]
                    ]
        , deconstructEffect = deconstructEffect
        }


all : Test
all =
    describe "TestContext (HTTP API)"
        [ describe "assertHttpRequest"
            [ test "can assert that an HTTP request was made from init (failure)" <|
                \() ->
                    start NoEffect
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/" }
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequest: Expected HTTP request (GET https://example.com/) to have been made, but it was not."
                                , "    No requests were made."
                                ]
                            )
            , test "can assert that an HTTP request was made from init (success)" <|
                \() ->
                    start (HttpGet "https://example.com/")
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/" }
                        |> expectSuccess
            , test "can assert that an HTTP request was made from update" <|
                \() ->
                    start NoEffect
                        |> TestContext.update (PassThroughEffect (HttpGet "https://example.com/from-update"))
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/from-update" }
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a user interaction" <|
                \() ->
                    start NoEffect
                        |> TestContext.clickButton "Get"
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/buttons/get" }
                        |> expectSuccess
            , test "error message includes list of pending requests" <|
                \() ->
                    start (HttpGet "https://example.com/actualRequest")
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/not-made" }
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequest: Expected HTTP request (GET https://example.com/not-made) to have been made, but it was not."
                                , "    The following requests were made:"
                                , "      - GET https://example.com/actualRequest"
                                ]
                            )
            , test "gives explanatory error when using assertHttpRequest without using createWithSimulatedEffects" <|
                \() ->
                    TestContext.create
                        { init = ( (), () )
                        , update = \() () -> ( (), () )
                        , view = \() -> Html.text "[view]"
                        }
                        |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/" }
                        |> expectFailure "TEST SETUP ERROR: In order to use assertHttpRequest, you MUST create your TestContext with TestContext.createWithSimulatedEvents"

            -- TODO: how to handle multiple requests made to the same method/URL?
            ]
        ]


expectSuccess : Expectation -> Expectation
expectSuccess actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.pass

        Just actualInfo ->
            Expect.fail ("Expected a success, but got a failure: " ++ actualInfo.description)


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> Expect.equal expectedFailureMessage
