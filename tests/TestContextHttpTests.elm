module TestContextHttpTests exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Events exposing (onClick)
import Json.Decode
import SimulatedEffect.Http as Http
import Test exposing (..)
import Test.Runner
import TestContext exposing (TestContext)


type TestEffect
    = NoEffect
    | HttpGet String
    | HttpPost String String


deconstructEffect : TestEffect -> List (TestContext.SimulatedEffect TestMsg)
deconstructEffect testEffect =
    case testEffect of
        NoEffect ->
            []

        HttpGet url ->
            [ Http.get
                { url = url
                , expect =
                    Http.expectJson HandleFriendsResponse
                        (Json.Decode.list Json.Decode.string)
                }
            ]

        HttpPost url body ->
            [ Http.post
                { url = url
                , body = Http.stringBody "application/json" body
                , expect = Http.expectString HandlePostResponse
                }
            ]


type TestMsg
    = PassThroughEffect TestEffect
    | HandleFriendsResponse (Result Http.Error (List String))
    | HandlePostResponse (Result Http.Error String)


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
                        ( Debug.toString result, NoEffect )

                    HandlePostResponse result ->
                        ( Debug.toString result, NoEffect )
        , view =
            \_ ->
                Html.div []
                    [ Html.button
                        [ onClick (PassThroughEffect (HttpGet "https://example.com/buttons/get")) ]
                        [ Html.text "Get" ]
                    ]
        }
        |> TestContext.withSimulatedEffects deconstructEffect
        |> TestContext.start ()


all : Test
all =
    describe "TestContext (HTTP API)"
        [ describe "assertHttpRequest"
            [ test "can assert that an HTTP request was made from init (failure)" <|
                \() ->
                    start NoEffect
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectFailure
                            (String.join "\n"
                                [ "assertHttpRequestWasMade: Expected HTTP request (GET https://example.com/) to have been made, but it was not."
                                , "    No requests were made."
                                ]
                            )
            , test "can assert that an HTTP request was made from init (success)" <|
                \() ->
                    start (HttpGet "https://example.com/")
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/"
                        |> expectSuccess
            , test "can assert that an HTTP request was made from update" <|
                \() ->
                    start NoEffect
                        |> TestContext.update (PassThroughEffect (HttpGet "https://example.com/from-update"))
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/from-update"
                        |> expectSuccess
            , test "can assert that an HTTP request was made via a user interaction" <|
                \() ->
                    start NoEffect
                        |> TestContext.clickButton "Get"
                        |> TestContext.assertHttpRequestWasMade "GET" "https://example.com/buttons/get"
                        |> expectSuccess
            , test "error message includes list of pending requests" <|
                \() ->
                    start (HttpGet "https://example.com/actualRequest")
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
                    start (HttpPost "https://example.com/ok" """{"ok":true}""")
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

            -- TODO: how to handle multiple requests made to the same method/URL?
            ]
        , describe "simulateHttpResponse"
            [ test "simulate OK response with valid JSON" <|
                \() ->
                    start (HttpGet "https://example.com/friends")
                        |> TestContext.simulateHttpSuccess "GET"
                            "https://example.com/friends"
                            """["Alex","Kelsey","Sam"]"""
                        |> TestContext.expectModel (Expect.equal """Ok ["Alex","Kelsey","Sam"]""")
            , test "simulate error response" <|
                \() ->
                    start (HttpGet "https://example.com/friends")
                        |> TestContext.simulateHttpResponse
                            { method = "GET"
                            , url = "https://example.com/friends"
                            }
                            { statusCode = 500
                            , body = ""
                            }
                        |> TestContext.expectModel (Expect.equal """Err (BadStatus 500)""")
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
