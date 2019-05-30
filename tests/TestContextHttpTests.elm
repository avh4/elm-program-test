module TestContextHttpTests exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Events exposing (onClick)
import Json.Decode
import SimulatedEffect.Http as Http
import SimulatedEffect.Task as Task
import Test exposing (..)
import Test.Http
import Test.Runner
import TestContext exposing (TestContext)


type alias TestEffect =
    List (TestContext.SimulatedEffect TestMsg)


type TestMsg
    = PassThroughEffect TestEffect
    | HandleFriendsResponse (Result Http.Error (List String))
    | HandleStringResponse (Result Http.Error String)


type alias TestModel =
    String


start : TestEffect -> TestContext TestMsg TestModel TestEffect
start initialEffect =
    TestContext.createElement
        { init = \() -> ( "Init", initialEffect )
        , update =
            \msg model ->
                case msg of
                    PassThroughEffect effect ->
                        ( model, effect )

                    HandleFriendsResponse result ->
                        ( Debug.toString result, [] )

                    HandleStringResponse result ->
                        ( Debug.toString result, [] )
        , view =
            \_ ->
                Html.div []
                    [ Html.button
                        [ onClick
                            (PassThroughEffect
                                [ Http.get
                                    { url = "https://example.com/buttons/get"
                                    , expect =
                                        Http.expectJson HandleFriendsResponse
                                            (Json.Decode.list Json.Decode.string)
                                    }
                                ]
                            )
                        ]
                        [ Html.text "Get" ]
                    ]
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


all : Test
all =
    describe "TestContext (HTTP API)"
        [ describe "assertHttpRequest"
            [ test "can assert that an HTTP request was made from init (failure)" <|
                \() ->
                    start []
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made, but it was not."
                                , "    No requests were made."
                                ]
                            )
            , test "can assert that an HTTP request was made from init (success)" <|
                \() ->
                    start [ Http.get { url = "https://example.com/", expect = Http.expectString HandleStringResponse } ]
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a Task" <|
                \() ->
                    start
                        [ Http.task
                            { method = "GET"
                            , headers = []
                            , url = "https://example.com/get"
                            , body = Http.emptyBody
                            , resolver = Http.stringResolver (\_ -> Ok "")
                            , timeout = Nothing
                            }
                            |> Task.attempt HandleStringResponse
                        ]
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/get"
                        |> expectSuccess
            , test "can assert that an HTTP request was made from update" <|
                \() ->
                    start []
                        |> TestContext.update (PassThroughEffect [ Http.get { url = "https://example.com/from-update", expect = Http.expectString HandleStringResponse } ])
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/from-update"
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a user interaction" <|
                \() ->
                    start []
                        |> TestContext.clickButton "Get"
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/buttons/get"
                        |> expectSuccess
            , test "error message includes list of pending requests" <|
                \() ->
                    start [ Http.get { url = "https://example.com/actualRequest", expect = Http.expectString HandleStringResponse } ]
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/not-made"
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/not-made) to have been made, but it was not."
                                , "    The following requests were made:"
                                , "      - GET https://example.com/actualRequest"
                                ]
                            )
            , test "gives explanatory error when using assertHttpRequest without using createWithSimulatedEffects" <|
                \() ->
                    TestContext.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view = \() -> Html.text "[view]"
                        }
                        |> TestContext.start ()
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure "TEST SETUP ERROR: In order to use assertHttpRequestWasMade, you MUST use TestContext.withSimulatedEffects before calling TestContext.start"
            , test "can assert on request body" <|
                \() ->
                    start
                        [ Http.post
                            { url = "https://example.com/ok"
                            , body = Http.stringBody "application/json" """{"ok":true}"""
                            , expect = Http.expectString HandleStringResponse
                            }
                        ]
                        |> TestContext.assertHttpRequest "POST"
                            "https://example.com/ok"
                            (.body >> Expect.equal """{"ok":900}""")
                        |> TestContext.done
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequest:"
                                , """"{\\"ok\\":true}\""""
                                , "╵"
                                , "│ Expect.equal"
                                , "╷"
                                , """"{\\"ok\\":900}\""""
                                ]
                            )
            , test "can assert on request headers" <|
                \() ->
                    start
                        [ Http.request
                            { method = "POST"
                            , headers = [ Http.header "X-Elm-Test" "Value 99" ]
                            , url = "https://example.com/ok"
                            , body = Http.stringBody "text/plain" "##"
                            , expect = Http.expectString HandleStringResponse
                            , timeout = Nothing
                            , tracker = Nothing
                            }
                        ]
                        |> TestContext.assertHttpRequest "POST"
                            "https://example.com/ok"
                            (Test.Http.hasHeader "Content-Type" "application/json")
                        |> TestContext.done
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequest:"
                                , "Expected HTTP header Content-Type: application/json"
                                , "but got headers:"
                                , "    Content-Type: text/plain"
                                , "    X-Elm-Test: Value 99"
                                ]
                            )

            -- TODO: how to handle multiple requests made to the same method/URL?
            ]
        , describe "simulateHttpResponse"
            [ test "simulate OK response with valid JSON" <|
                \() ->
                    start
                        [ Http.get
                            { url = "https://example.com/friends"
                            , expect =
                                Http.expectJson HandleFriendsResponse
                                    (Json.Decode.list Json.Decode.string)
                            }
                        ]
                        |> TestContext.simulateHttpSuccess "GET"
                            "https://example.com/friends"
                            """["Alex","Kelsey","Sam"]"""
                        |> TestContext.expectModel (Expect.equal """Ok ["Alex","Kelsey","Sam"]""")
            , test "simulate error response" <|
                \() ->
                    start [ Http.get { url = "https://example.com/friends", expect = Http.expectString HandleStringResponse } ]
                        |> TestContext.simulateHttpResponse "GET"
                            "https://example.com/friends"
                            { statusCode = 500
                            , body = ""
                            }
                        |> TestContext.expectModel (Expect.equal """Err (BadStatus 500)""")
            , test "can resolve a chain of requests" <|
                \() ->
                    start
                        [ Http.task
                            { method = "GET"
                            , headers = []
                            , url = "https://example.com/A"
                            , body = Http.emptyBody
                            , resolver = Http.stringResolver (\_ -> Ok "A-return")
                            , timeout = Nothing
                            }
                            |> Task.andThen
                                (\aResult ->
                                    Http.task
                                        { method = "GET"
                                        , headers = []
                                        , url = "https://example.com/B/" ++ aResult
                                        , body = Http.emptyBody
                                        , resolver = Http.stringResolver (\_ -> Ok "B-return")
                                        , timeout = Nothing
                                        }
                                )
                            |> Task.andThen
                                (\bResult ->
                                    Http.task
                                        { method = "GET"
                                        , headers = []
                                        , url = "https://example.com/C/" ++ bResult
                                        , body = Http.emptyBody
                                        , resolver = Http.stringResolver (\_ -> Ok "C-return")
                                        , timeout = Nothing
                                        }
                                )
                            |> Task.attempt HandleStringResponse
                        ]
                        |> TestContext.simulateHttpSuccess "GET"
                            "https://example.com/A"
                            """{}"""
                        |> TestContext.simulateHttpSuccess "GET"
                            "https://example.com/B/A-return"
                            """{}"""
                        |> TestContext.simulateHttpSuccess "GET"
                            "https://example.com/C/B-return"
                            """{}"""
                        |> TestContext.expectModel (Expect.equal """Ok "C-return\"""")
            ]
        ]


expectSuccess : Expectation -> Expectation
expectSuccess actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.pass

        Just actualInfo ->
            Expect.fail ("expectSuccess: Expected a success, but got a failure:\n" ++ actualInfo.description)


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> Expect.equal expectedFailureMessage
