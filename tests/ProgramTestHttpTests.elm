module ProgramTestHttpTests exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Events exposing (onClick)
import Json.Decode
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Task as Task
import Test exposing (..)
import Test.Expect exposing (expectFailure, expectSuccess)
import Test.Http
import TestHelper exposing (..)


type alias TestEffect =
    ProgramTest.SimulatedEffect TestMsg


type TestMsg
    = PassThroughEffect TestEffect
    | HandleFriendsResponse (Result Http.Error (List String))
    | HandleStringResponse (Result Http.Error String)


type alias TestModel =
    String


start : TestEffect -> ProgramTest TestModel TestMsg TestEffect
start initialEffect =
    ProgramTest.createElement
        { init = \() -> ( "Init", initialEffect )
        , update =
            \msg model ->
                case msg of
                    PassThroughEffect effect ->
                        ( model, effect )

                    HandleFriendsResponse result ->
                        ( model ++ ";" ++ Debug.toString result, SimulatedEffect.Cmd.none )

                    HandleStringResponse result ->
                        ( model ++ ";" ++ Debug.toString result, SimulatedEffect.Cmd.none )
        , view =
            \_ ->
                Html.div []
                    [ Html.button
                        [ onClick
                            (PassThroughEffect
                                (Http.get
                                    { url = "https://example.com/buttons/get"
                                    , expect =
                                        Http.expectJson HandleFriendsResponse
                                            (Json.Decode.list Json.Decode.string)
                                    }
                                )
                            )
                        ]
                        [ Html.text "Get" ]
                    ]
        }
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.start ()


all : Test
all =
    describe "ProgramTest (HTTP API)" <|
        let
            testRequestWasMade =
                testAssertion2
                    ProgramTest.expectHttpRequestWasMade
                    ProgramTest.ensureHttpRequestWasMade

            testRequest =
                testAssertion3
                    ProgramTest.expectHttpRequest
                    ProgramTest.ensureHttpRequest

            testRequests =
                testAssertion3
                    ProgramTest.expectHttpRequests
                    ProgramTest.ensureHttpRequests
        in
        [ describe "assertHttpRequest"
            [ testRequestWasMade "can assert that an HTTP request was made from init (failure)" <|
                \expect assertHttpRequestWasMade ->
                    start SimulatedEffect.Cmd.none
                        |> assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ expect ++ "HttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made and still be pending, but no such requests were made."
                            , "    No requests were made."
                            ]
            , testRequestWasMade "can assert that an HTTP request was made from init (success)" <|
                \expect assertHttpRequestWasMade ->
                    start (Http.get { url = "https://example.com/", expect = Http.expectString HandleStringResponse })
                        |> assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectSuccess
            , testRequestWasMade "can assert that an HTTP request was made via a Task" <|
                \expect assertHttpRequestWasMade ->
                    start
                        (Http.task
                            { method = "GET"
                            , headers = []
                            , url = "https://example.com/get"
                            , body = Http.emptyBody
                            , resolver = Http.stringResolver (\_ -> Ok "")
                            , timeout = Nothing
                            }
                            |> Task.attempt HandleStringResponse
                        )
                        |> assertHttpRequestWasMade "GET" "https://example.com/get"
                        |> expectSuccess
            , testRequestWasMade "can assert that an HTTP request was made from update" <|
                \expect assertHttpRequestWasMade ->
                    start SimulatedEffect.Cmd.none
                        |> ProgramTest.update (PassThroughEffect (Http.get { url = "https://example.com/from-update", expect = Http.expectString HandleStringResponse }))
                        |> assertHttpRequestWasMade "GET" "https://example.com/from-update"
                        |> expectSuccess
            , testRequestWasMade "can assert that an HTTP request was made via a user interaction" <|
                \expect assertHttpRequestWasMade ->
                    start SimulatedEffect.Cmd.none
                        |> ProgramTest.clickButton "Get"
                        |> assertHttpRequestWasMade "GET" "https://example.com/buttons/get"
                        |> expectSuccess
            , testRequestWasMade "error message includes list of pending requests" <|
                \expect assertHttpRequestWasMade ->
                    start (Http.get { url = "https://example.com/actualRequest", expect = Http.expectString HandleStringResponse })
                        |> assertHttpRequestWasMade "GET" "https://example.com/not-made"
                        |> expectFailure
                            [ expect ++ "HttpRequestWasMade: Expected HTTP request (GET https://example.com/not-made) to have been made and still be pending, but no such requests were made."
                            , "    The following requests were made:"
                            , "      - GET https://example.com/actualRequest"
                            ]
            , testRequestWasMade "gives explanatory error when used without `withSimulatedEffects`" <|
                \expect assertHttpRequestWasMade ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view = \() -> Html.text "[view]"
                        }
                        |> ProgramTest.start ()
                        |> assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ "TEST SETUP ERROR: In order to use " ++ expect ++ "HttpRequestWasMade, you MUST use ProgramTest.withSimulatedEffects before calling ProgramTest.start"
                            ]
            , testRequest "can assert on request body" <|
                \expect assertHttpRequest ->
                    start
                        (Http.post
                            { url = "https://example.com/ok"
                            , body = Http.stringBody "application/json" """{"ok":true}"""
                            , expect = Http.expectString HandleStringResponse
                            }
                        )
                        |> assertHttpRequest "POST"
                            "https://example.com/ok"
                            (.body >> Expect.equal """{"ok":900}""")
                        |> expectFailure
                            [ expect ++ "HttpRequest:"
                            , "         ▼▼▼▼  "
                            , """"{\\"ok\\":true}\""""
                            , "╷"
                            , "│ Expect.equal"
                            , "╵"
                            , """"{\\"ok\\":900}\""""
                            , "         ▲▲▲  "
                            ]
            , testRequest "can assert on request headers" <|
                \expect assertHttpRequest ->
                    start
                        (Http.request
                            { method = "POST"
                            , headers = [ Http.header "X-Elm-Test" "Value 99" ]
                            , url = "https://example.com/ok"
                            , body = Http.stringBody "text/plain" "##"
                            , expect = Http.expectString HandleStringResponse
                            , timeout = Nothing
                            , tracker = Nothing
                            }
                        )
                        |> assertHttpRequest "POST"
                            "https://example.com/ok"
                            (.headers
                                >> Expect.equal
                                    [ ( "Content-Type", "text/plain" )
                                    , ( "X-Elm-Test", "Value 99" )
                                    ]
                            )
            , testRequestWasMade "two identical requests give an error" <|
                \expect assertHttpRequestWasMade ->
                    start
                        (SimulatedEffect.Cmd.batch
                            [ Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            ]
                        )
                        |> assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ expect ++ "HttpRequestWasMade: Expected a single HTTP request (GET https://example.com/) to have been made, but 2 such requests were made."
                            , "    The following requests were made:"
                            , "      - GET https://example.com/"
                            , "      - GET https://example.com/"
                            , ""
                            , "NOTE: If you want to allow multiple requests to the same endpoint, use ProgramTest." ++ expect ++ "HttpRequests."
                            ]
            , testRequests "can assert that no requests were made" <|
                \expect assertHttpRequests ->
                    start SimulatedEffect.Cmd.none
                        |> assertHttpRequests "GET" "https://example.com/" (Expect.equal [])
            , testRequests "can assert multiple requests were made to the same endpoint" <|
                \expect assertHttpRequests ->
                    start
                        (SimulatedEffect.Cmd.batch
                            [ Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            ]
                        )
                        |> assertHttpRequests "GET" "https://example.com/" (List.length >> Expect.equal 2)
            ]
        , describe "simulateHttpResponse"
            [ test "simulate OK response with valid JSON" <|
                \() ->
                    start
                        (Http.get
                            { url = "https://example.com/friends"
                            , expect =
                                Http.expectJson HandleFriendsResponse
                                    (Json.Decode.list Json.Decode.string)
                            }
                        )
                        |> ProgramTest.simulateHttpOk "GET"
                            "https://example.com/friends"
                            """["Alex","Kelsey","Sam"]"""
                        |> ProgramTest.expectModel (Expect.equal """Init;Ok ["Alex","Kelsey","Sam"]""")
            , test "simulate error response" <|
                \() ->
                    start (Http.get { url = "https://example.com/friends", expect = Http.expectString HandleStringResponse })
                        |> ProgramTest.simulateHttpResponse "GET"
                            "https://example.com/friends"
                            (Test.Http.httpResponse
                                { statusCode = 500
                                , headers = []
                                , body = ""
                                }
                            )
                        |> ProgramTest.expectModel (Expect.equal """Init;Err (BadStatus 500)""")
            , test "can simulate network error" <|
                \() ->
                    start (Http.get { url = "https://example.com/friends", expect = Http.expectString HandleStringResponse })
                        |> ProgramTest.simulateHttpResponse "GET"
                            "https://example.com/friends"
                            Test.Http.networkError
                        |> ProgramTest.expectModel (Expect.equal """Init;Err NetworkError""")
            , test "can resolve a chain of requests" <|
                \() ->
                    start
                        (Http.task
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
                        )
                        |> ProgramTest.simulateHttpOk "GET"
                            "https://example.com/A"
                            """{}"""
                        |> ProgramTest.simulateHttpOk "GET"
                            "https://example.com/B/A-return"
                            """{}"""
                        |> ProgramTest.simulateHttpOk "GET"
                            "https://example.com/C/B-return"
                            """{}"""
                        |> ProgramTest.expectModel (Expect.equal """Init;Ok "C-return\"""")
            , testRequestWasMade "a request can only be resolved once" <|
                \expect assertHttpRequestWasMade ->
                    start
                        (Http.get
                            { url = "https://example.com/"
                            , expect = Http.expectString HandleStringResponse
                            }
                        )
                        |> ProgramTest.simulateHttpOk "GET" "https://example.com/" """{}"""
                        |> assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ expect ++ "HttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made and still be pending, but no such requests were made."
                            , "    No requests were made."
                            ]
            , test "two identical requests give an error" <|
                \() ->
                    start
                        (SimulatedEffect.Cmd.batch
                            [ Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            ]
                        )
                        |> ProgramTest.simulateHttpOk "GET" "https://example.com/" """{}"""
                        |> ProgramTest.done
                        |> expectFailure
                            [ "simulateHttpOk: Expected a single HTTP request (GET https://example.com/) to have been made, but 2 such requests were made."
                            , "    The following requests were made:"
                            , "      - GET https://example.com/"
                            , "      - GET https://example.com/"
                            , ""
                            , "NOTE: If you want to allow multiple requests to the same endpoint, use ProgramTest.simulateHttpResponseAdvanced."
                            ]
            , test "can resolve one of multiple identical requests" <|
                \() ->
                    start
                        (SimulatedEffect.Cmd.batch
                            [ Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            ]
                        )
                        |> ProgramTest.simulateHttpResponseAdvanced "GET" "https://example.com/" 2 (Test.Http.httpResponse { statusCode = 200, headers = [], body = "A" })
                        |> ProgramTest.simulateHttpResponseAdvanced "GET" "https://example.com/" 2 (Test.Http.httpResponse { statusCode = 200, headers = [], body = "B" })
                        |> ProgramTest.simulateHttpResponseAdvanced "GET" "https://example.com/" 1 (Test.Http.httpResponse { statusCode = 200, headers = [], body = "C" })
                        |> ProgramTest.expectModel (Expect.equal """Init;Ok "A";Ok "B";Ok "C\"""")
            , test "gives an error trying to resolve one of multiple when not enough requests have been made" <|
                \() ->
                    start
                        (SimulatedEffect.Cmd.batch
                            [ Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            , Http.get
                                { url = "https://example.com/"
                                , expect = Http.expectString HandleStringResponse
                                }
                            ]
                        )
                        |> ProgramTest.simulateHttpResponseAdvanced "GET" "https://example.com/" 4 (Test.Http.httpResponse { statusCode = 200, headers = [], body = "" })
                        |> ProgramTest.done
                        |> expectFailure
                            [ "simulateHttpResponseAdvanced: Expected at least 4 HTTP requests (GET https://example.com/) to have been made and still be pending, but only 2 such requests were made."
                            , "    The following requests were made:"
                            , "      - GET https://example.com/"
                            , "      - GET https://example.com/"
                            ]
            ]
        ]
