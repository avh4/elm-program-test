module Test.HttpTests exposing (all)

import SimulatedEffect.Http as Http
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import Test.Http
import TestContext
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
                    |> TestContext.assertHttpRequest "GET"
                        "https://example.com/ok"
                        (Test.Http.hasHeader "Content-Type" "application/json")
                    |> TestContext.done
                    |> expectFailure
                        [ "assertHttpRequest:"
                        , "Expected HTTP header Content-Type: application/json"
                        , "but got headers:"
                        , "    X-Elm-Test: Value 99"
                        ]
        ]
