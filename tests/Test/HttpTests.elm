module Test.HttpTests exposing (all)

import Expect
import Json.Decode
import Json.Encode
import ProgramTest
import SimulatedEffect.Http as Http
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import Test.Http
import TestingProgram exposing (Msg(..))


start =
    TestingProgram.startEffects


all : Test
all =
    describe "Test.Http"
        [ test "can assert on request headers" <|
            \() ->
                start
                    (Http.request
                        { method = "GET"
                        , headers = [ Http.header "X-Elm-Test" "Value 99" ]
                        , url = "https://example.com/ok"
                        , body = Http.emptyBody
                        , expect = Http.expectWhatever (Debug.toString >> Log)
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )
                    |> ProgramTest.assertHttpRequest "GET"
                        "https://example.com/ok"
                        (Test.Http.hasHeader "Content-Type" "application/json")
                    |> ProgramTest.done
                    |> expectFailure
                        [ "assertHttpRequest:"
                        , "Expected HTTP header Content-Type: application/json"
                        , "but got headers:"
                        , "    X-Elm-Test: Value 99"
                        ]
        , test "can assert on JSON body" <|
            \() ->
                start
                    (Http.post
                        { url = "https://example.com/ok"
                        , body =
                            Http.jsonBody
                                (Json.Encode.object
                                    [ ( "a", Json.Encode.int 8 )
                                    ]
                                )
                        , expect = Http.expectWhatever (Debug.toString >> Log)
                        }
                    )
                    |> ProgramTest.assertHttpRequest "POST"
                        "https://example.com/ok"
                        (Test.Http.expectJsonBody
                            (Json.Decode.field "a" Json.Decode.int)
                            (Expect.equal 8)
                        )
                    |> ProgramTest.done
        ]
