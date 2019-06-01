module TestContextTests.SimulatedEffects.PortsTest exposing (all)

import Expect exposing (Expectation)
import Html
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
                        |> expectFailure
                            [ "checkAndClearOutgoingPort: values sent to port \"other\" did not match:"
                            , "[]"
                            , "╵"
                            , "│ Expect.equal"
                            , "╷"
                            , "[()]"
                            ]
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
                        |> expectFailure
                            [ "checkAndClearOutgoingPort: failed to decode port values: Problem with the given value:"
                            , ""
                            , "5"
                            , ""
                            , "Expecting a STRING"
                            ]
            ]
        , describe "incoming ports" <|
            let
                startSub f =
                    TestContext.createElement
                        { init = \() -> ( [], [] )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> TestContext.withSimulatedEffects identity
                        |> TestContext.withSimulatedSubscriptions f
                        |> TestContext.start ()
            in
            [ test "simulates incoming ports" <|
                \() ->
                    startSub (\_ -> [ SimulatedEffect.Port.subscribe "int" Decode.int (Debug.toString >> Log) ])
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.expectModel (Expect.equal [ "7" ])
            , test "shows useful error the port is not subscribed to" <|
                \() ->
                    startSub (\_ -> [])
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.done
                        |> expectFailure [ "simulateIncomingPort \"int\": the program is not currently subscribed to the port" ]
            , test "shows useful error when withSimulatedSubscriptions wasn't used" <|
                \() ->
                    TestContext.createElement
                        { init = \() -> ( [], [] )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> TestContext.withSimulatedEffects identity
                        |> TestContext.start ()
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.done
                        |> expectFailure [ "simulateIncomingPort \"int\": you MUST use TestContext.withSimulatedSubscriptions to be able to use simulateIncomingPort" ]
            , test "shows useful error when decoder fails" <|
                \() ->
                    startSub (\_ -> [ SimulatedEffect.Port.subscribe "int" Decode.int (Debug.toString >> Log) ])
                        |> TestContext.simulateIncomingPort "int" (Json.object [])
                        |> TestContext.done
                        |> expectFailure
                            [ "simulateIncomingPort \"int\": the value provided does not match the type that the port is expecting:"
                            , "Problem with the given value:"
                            , ""
                            , "{}"
                            , ""
                            , "Expecting an INT"
                            ]
            ]
        ]


expectFailure : List String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> Expect.equal (String.join "\n" expectedFailureMessage)
