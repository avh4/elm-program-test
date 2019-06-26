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


type alias TestEffect =
    ProgramTest.SimulatedEffect TestMsg


type TestMsg
    = PassThroughEffect TestEffect
    | HandleFriendsResponse (Result Http.Error (List String))
    | HandleStringResponse (Result Http.Error String)


type alias TestModel =
    String


start : TestEffect -> ProgramTest TestMsg TestModel TestEffect
start initialEffect =
    ProgramTest.createElement
        { init = \() -> ( "Init", initialEffect )
        , update =
            \msg model ->
                case msg of
                    PassThroughEffect effect ->
                        ( model, effect )

                    HandleFriendsResponse result ->
                        ( Debug.toString result, SimulatedEffect.Cmd.none )

                    HandleStringResponse result ->
                        ( Debug.toString result, SimulatedEffect.Cmd.none )
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
    describe "ProgramTest (HTTP API)"
        [ describe "assertHttpRequest"
            [ test "can assert that an HTTP request was made from init (failure)" <|
                \() ->
                    start SimulatedEffect.Cmd.none
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made, but it was not."
                            , "    No requests were made."
                            ]
            , test "can assert that an HTTP request was made from init (success)" <|
                \() ->
                    start (Http.get { url = "https://example.com/", expect = Http.expectString HandleStringResponse })
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a Task" <|
                \() ->
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
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/get"
                        |> expectSuccess
            , test "can assert that an HTTP request was made from update" <|
                \() ->
                    start SimulatedEffect.Cmd.none
                        |> ProgramTest.update (PassThroughEffect (Http.get { url = "https://example.com/from-update", expect = Http.expectString HandleStringResponse }))
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/from-update"
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a user interaction" <|
                \() ->
                    start SimulatedEffect.Cmd.none
                        |> ProgramTest.clickButton "Get"
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/buttons/get"
                        |> expectSuccess
            , test "error message includes list of pending requests" <|
                \() ->
                    start (Http.get { url = "https://example.com/actualRequest", expect = Http.expectString HandleStringResponse })
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/not-made"
                        |> expectFailure
                            [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/not-made) to have been made, but it was not."
                            , "    The following requests were made:"
                            , "      - GET https://example.com/actualRequest"
                            ]
            , test "gives explanatory error when using assertHttpRequest without using withSimulatedEffects" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view = \() -> Html.text "[view]"
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ "TEST SETUP ERROR: In order to use assertHttpRequestWasMade, you MUST use ProgramTest.withSimulatedEffects before calling ProgramTest.start"
                            ]
            , test "can assert on request body" <|
                \() ->
                    start
                        (Http.post
                            { url = "https://example.com/ok"
                            , body = Http.stringBody "application/json" """{"ok":true}"""
                            , expect = Http.expectString HandleStringResponse
                            }
                        )
                        |> ProgramTest.assertHttpRequest "POST"
                            "https://example.com/ok"
                            (.body >> Expect.equal """{"ok":900}""")
                        |> ProgramTest.done
                        |> expectFailure
                            [ "assertHttpRequest:"
                            , """"{\\"ok\\":true}\""""
                            , "╵"
                            , "│ Expect.equal"
                            , "╷"
                            , """"{\\"ok\\":900}\""""
                            ]
            , test "can assert on request headers" <|
                \() ->
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
                        |> ProgramTest.assertHttpRequest "POST"
                            "https://example.com/ok"
                            (.headers
                                >> Expect.equal
                                    [ ( "Content-Type", "text/plain" )
                                    , ( "X-Elm-Test", "Value 99" )
                                    ]
                            )
                        |> ProgramTest.done

            -- TODO: how to handle multiple requests made to the same method/URL?
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
                        |> ProgramTest.expectModel (Expect.equal """Ok ["Alex","Kelsey","Sam"]""")
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
                        |> ProgramTest.expectModel (Expect.equal """Err (BadStatus 500)""")
            , test "can simulate network error" <|
                \() ->
                    start (Http.get { url = "https://example.com/friends", expect = Http.expectString HandleStringResponse })
                        |> ProgramTest.simulateHttpResponse "GET"
                            "https://example.com/friends"
                            Test.Http.networkError
                        |> ProgramTest.expectModel (Expect.equal """Err NetworkError""")
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
                        |> ProgramTest.expectModel (Expect.equal """Ok "C-return\"""")
            , test "a request can only be resolved once" <|
                \() ->
                    start
                        (Http.get
                            { url = "https://example.com/"
                            , expect = Http.expectString HandleStringResponse
                            }
                        )
                        |> ProgramTest.simulateHttpOk "GET" "https://example.com/" """{}"""
                        |> ProgramTest.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made, but it was not."
                            , "    No requests were made."
                            ]
            ]
        ]
