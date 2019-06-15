module TestContextTests.SimulatedEffects.PortsTest exposing (all)

import Expect exposing (Expectation)
import Html
import Json.Decode as Decode
import Json.Encode as Json
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)
import TestingProgram exposing (Msg(..))


start =
    TestingProgram.startEffects


all : Test
all =
    describe "simulated port effects"
        [ describe "outgoing ports"
            [ test "can check sent values" <|
                \() ->
                    start (SimulatedEffect.Ports.send "unit" Json.null)
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.done
            , test "gives error if checked values don't match" <|
                \() ->
                    start (SimulatedEffect.Ports.send "unit" Json.null)
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
                    start (SimulatedEffect.Ports.send "unit" Json.null)
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [])
                        |> TestContext.done
            , test "records values in correct order" <|
                \() ->
                    start (SimulatedEffect.Ports.send "int" (Json.int 5))
                        |> TestContext.update (ProduceEffects (SimulatedEffect.Ports.send "int" (Json.int 7)))
                        |> TestContext.checkAndClearOutgoingPort "int" Decode.int (Expect.equal [ 5, 7 ])
                        |> TestContext.done
            , test "shows useful error when decoding fails" <|
                \() ->
                    start (SimulatedEffect.Ports.send "int" (Json.int 5))
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
                        { init = \() -> ( [], SimulatedEffect.Cmd.none )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> TestContext.withSimulatedEffects identity
                        |> TestContext.withSimulatedSubscriptions f
                        |> TestContext.start ()
            in
            [ test "simulates incoming ports" <|
                \() ->
                    startSub (\_ -> [ SimulatedEffect.Ports.subscribe "int" Decode.int (Debug.toString >> Log) ])
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.expectModel (Expect.equal [ "7" ])
            , test "shows useful error when the port is not subscribed to" <|
                \() ->
                    startSub (\_ -> [])
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.done
                        |> expectFailure [ "simulateIncomingPort \"int\": the program is not currently subscribed to the port" ]
            , test "shows useful error when withSimulatedSubscriptions wasn't used" <|
                \() ->
                    TestContext.createElement
                        { init = \() -> ( [], SimulatedEffect.Cmd.none )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> TestContext.start ()
                        |> TestContext.simulateIncomingPort "int" (Json.int 7)
                        |> TestContext.done
                        |> expectFailure [ "simulateIncomingPort \"int\": you MUST use TestContext.withSimulatedSubscriptions to be able to use simulateIncomingPort" ]
            , test "shows useful error when decoder fails" <|
                \() ->
                    startSub (\_ -> [ SimulatedEffect.Ports.subscribe "int" Decode.int (Debug.toString >> Log) ])
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
