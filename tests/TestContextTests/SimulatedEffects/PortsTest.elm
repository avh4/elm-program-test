module TestContextTests.SimulatedEffects.PortsTest exposing (all)

import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Json
import SimulatedEffect.Port
import Test exposing (..)
import Test.Runner
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)
import TestingProgram exposing (Msg(..), start)


all : Test
all =
    describe "simulated port effects"
        [ describe "outgoing ports"
            [ test "can check sent values" <|
                \() ->
                    start [ SimulatedEffect.Port.send "unit" Json.null ]
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.done
            , test "gives error if checked values don't match" <|
                \() ->
                    start [ SimulatedEffect.Port.send "unit" Json.null ]
                        |> TestContext.checkAndClearOutgoingPort "other" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.done
                        |> expectFailure "checkAndClearOutgoingPort: values sent to port \"other\" did not match:\n[]\n╵\n│ Expect.equal\n╷\n[()]"
            , test "clears values after checking" <|
                \() ->
                    start [ SimulatedEffect.Port.send "unit" Json.null ]
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [])
                        |> TestContext.done
            , test "records values in correct order" <|
                \() ->
                    start [ SimulatedEffect.Port.send "int" (Json.int 5) ]
                        |> TestContext.update (ProduceEffects [ SimulatedEffect.Port.send "int" (Json.int 7) ])
                        |> TestContext.checkAndClearOutgoingPort "int" Decode.int (Expect.equal [ 5, 7 ])
                        |> TestContext.done
            , test "shows useful error when decoding fails" <|
                \() ->
                    start [ SimulatedEffect.Port.send "int" (Json.int 5) ]
                        |> TestContext.checkAndClearOutgoingPort "int" Decode.string (Expect.equal [])
                        |> TestContext.done
                        |> expectFailure "checkAndClearOutgoingPort: failed to decode port values: Problem with the given value:\n\n5\n\nExpecting a STRING"
            ]
        ]


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> Expect.equal expectedFailureMessage
