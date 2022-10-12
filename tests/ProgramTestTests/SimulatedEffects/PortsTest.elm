module ProgramTestTests.SimulatedEffects.PortsTest exposing (all)

import Expect exposing (Expectation)
import Html
import Json.Decode as Decode
import Json.Encode as Json
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import SimulatedEffect.Sub
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import TestHelper exposing (..)
import TestingProgram exposing (Msg(..))


start =
    TestingProgram.startEffects


all : Test
all =
    describe "simulated port effects" <|
        let
            testOutgoingPortValues =
                testAssertion3
                    ProgramTest.expectOutgoingPortValues
                    ProgramTest.ensureOutgoingPortValues
        in
        [ describe "outgoing ports"
            [ testOutgoingPortValues "can check sent values" <|
                \_ assertOutgoingPortValues ->
                    start (SimulatedEffect.Ports.send "unit" Json.null)
                        |> assertOutgoingPortValues "unit" (Decode.null ()) (Expect.equal [ () ])
            , testOutgoingPortValues "gives error if checked values don't match" <|
                \assert assertOutgoingPortValues ->
                    start (SimulatedEffect.Ports.send "unit" Json.null)
                        |> assertOutgoingPortValues "other" (Decode.null ()) (Expect.equal [ () ])
                        |> expectFailure
                            [ assert ++ "OutgoingPortValues: values sent to port \"other\" did not match:"
                            , "  "
                            , "[]"
                            , "╷"
                            , "│ Expect.equal"
                            , "╵"
                            , "[()]"
                            , " ▲▲ "
                            ]
            , testOutgoingPortValues "clears values after checking" <|
                \_ assertOutgoingPortValues ->
                    start (SimulatedEffect.Ports.send "unit" Json.null)
                        |> ProgramTest.ensureOutgoingPortValues "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> assertOutgoingPortValues "unit" (Decode.null ()) (Expect.equal [])
            , testOutgoingPortValues "records values in correct order" <|
                \_ assertOutgoingPortValues ->
                    start (SimulatedEffect.Ports.send "int" (Json.int 5))
                        |> ProgramTest.update (ProduceEffects (SimulatedEffect.Ports.send "int" (Json.int 7)))
                        |> assertOutgoingPortValues "int" Decode.int (Expect.equal [ 5, 7 ])
            , testOutgoingPortValues "shows useful error when decoding fails" <|
                \assert assertOutgoingPortValues ->
                    start (SimulatedEffect.Ports.send "int" (Json.int 5))
                        |> assertOutgoingPortValues "int" Decode.string (Expect.equal [])
                        |> expectFailure
                            [ assert ++ "OutgoingPortValues: failed to decode port values: Problem with the given value:"
                            , ""
                            , "5"
                            , ""
                            , "Expecting a STRING"
                            ]
            ]
        , describe "incoming ports" <|
            let
                startSub f =
                    ProgramTest.createElement
                        { init = \() -> ( [], SimulatedEffect.Cmd.none )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> ProgramTest.withSimulatedEffects identity
                        |> ProgramTest.withSimulatedSubscriptions f
                        |> ProgramTest.start ()
            in
            [ test "simulates incoming ports" <|
                \() ->
                    startSub (\_ -> SimulatedEffect.Ports.subscribe "int" Decode.int (Debug.toString >> Log))
                        |> ProgramTest.simulateIncomingPort "int" (Json.int 7)
                        |> ProgramTest.expectModel (Expect.equal [ "7" ])
            , test "shows useful error when the port is not subscribed to" <|
                \() ->
                    startSub (\_ -> SimulatedEffect.Sub.none)
                        |> ProgramTest.simulateIncomingPort "int" (Json.int 7)
                        |> ProgramTest.done
                        |> expectFailure [ "simulateIncomingPort \"int\": the program is not currently subscribed to the port" ]
            , test "shows useful error when withSimulatedSubscriptions wasn't used" <|
                \() ->
                    ProgramTest.createElement
                        { init = \() -> ( [], SimulatedEffect.Cmd.none )
                        , update = TestingProgram.update
                        , view = \_ -> Html.text ""
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.simulateIncomingPort "int" (Json.int 7)
                        |> ProgramTest.done
                        |> expectFailure [ "simulateIncomingPort \"int\": you MUST use ProgramTest.withSimulatedSubscriptions to be able to use simulateIncomingPort" ]
            , test "shows useful error when decoder fails" <|
                \() ->
                    startSub (\_ -> SimulatedEffect.Ports.subscribe "int" Decode.int (Debug.toString >> Log))
                        |> ProgramTest.simulateIncomingPort "int" (Json.object [])
                        |> ProgramTest.done
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
