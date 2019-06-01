module TestContextTests.SimulatedEffects.PortsTest exposing (all)

import Expect exposing (Expectation)
import Html
import Json.Decode as Decode
import Json.Encode as Json
import SimulatedEffect.Cmd
import SimulatedEffect.Port
import Test exposing (..)
import Test.Runner
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)


startEffect :
    SimulatedEffect m
    -> TestContext ( String, List (SimulatedEffect m) ) String (List (SimulatedEffect ( String, List (SimulatedEffect m) )))
startEffect initialEffect =
    TestContext.createElement
        { init = \() -> ( "", List.map (SimulatedEffect.Cmd.map (\r -> ( Debug.toString r, [] ))) [ initialEffect ] )
        , update = \( r, e ) model -> ( model ++ ";" ++ r, List.map (SimulatedEffect.Cmd.map (\r_ -> ( Debug.toString r_, [] ))) e )
        , view = \_ -> Html.text ""
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


all : Test
all =
    describe "simulated port effects"
        [ describe "outgoing ports"
            [ test "can check sent values" <|
                \() ->
                    startEffect (SimulatedEffect.Port.send "unit" Json.null)
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.done
            , test "gives error if checked values don't match" <|
                \() ->
                    startEffect (SimulatedEffect.Port.send "unit" Json.null)
                        |> TestContext.checkAndClearOutgoingPort "other" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.done
                        |> expectFailure "checkAndClearOutgoingPort: values sent to port \"other\" did not match:\n[]\n╵\n│ Expect.equal\n╷\n[()]"
            , test "clears values after checking" <|
                \() ->
                    startEffect (SimulatedEffect.Port.send "unit" Json.null)
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [ () ])
                        |> TestContext.checkAndClearOutgoingPort "unit" (Decode.null ()) (Expect.equal [])
                        |> TestContext.done
            , test "records values in correct order" <|
                \() ->
                    startEffect (SimulatedEffect.Port.send "int" (Json.int 5))
                        |> TestContext.update ( "*", [ SimulatedEffect.Port.send "int" (Json.int 7) ] )
                        |> TestContext.checkAndClearOutgoingPort "int" Decode.int (Expect.equal [ 5, 7 ])
                        |> TestContext.done
            , test "shows useful error when decoding fails" <|
                \() ->
                    startEffect (SimulatedEffect.Port.send "int" (Json.int 5))
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
