module TestContextHttpTests exposing (all)

import Expect exposing (Expectation)
import Html
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


type alias TestMsg =
    TestEffect


start : TestEffect -> TestContext TestMsg () TestEffect
start initialEffect =
    TestContext.createWithSimulatedEffects
        { init = ( (), initialEffect )
        , update = \msg () -> ( (), msg )
        , view = \() -> Html.text "[view]"
        , deconstructEffect = deconstructEffect
        }


all : Test
all =
    describe "TestContext (HTTP API)"
        [ test "can assert that an HTTP request was made from init (failure)" <|
            \() ->
                start NoEffect
                    |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/" }
                    |> expectFailure "assertHttpRequest: Expected HTTP request (GET https://example.com/) to have been made, but it was not"
        , test "can assert that an HTTP request was made from init (success)" <|
            \() ->
                start (HttpGet "https://example.com/")
                    |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/" }
                    |> expectSuccess
        , test "can assert that an HTTP request was made from update" <|
            \() ->
                start NoEffect
                    |> TestContext.update (HttpGet "https://example.com/from-update")
                    |> TestContext.assertHttpRequest { method = "GET", url = "https://example.com/from-update" }
                    |> expectSuccess

        -- TODO: verify that simulating user input triggers the update code path
        -- TODO: error message includes list of pending requests
        -- TODO: how to handle multiple requests made to the same method/URL?
        -- TODO: give specicif error message if `createWithSimulatedEffects` was not used
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
