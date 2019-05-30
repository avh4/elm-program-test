module TestContextTests.SimulatedEffects.TimeTest exposing (all)

import Expect
import Html
import SimulatedEffect.Process as Process
import SimulatedEffect.Task as Task
import Test exposing (..)
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)


startTask :
    List (SimulatedTask x a)
    -> TestContext ( String, List (SimulatedTask x a) ) String (List (SimulatedEffect ( String, List (SimulatedTask x a) )))
startTask initialTasks =
    TestContext.createElement
        { init = \() -> ( "", List.map (Task.attempt (\r -> ( Debug.toString r, [] ))) initialTasks )
        , update = \( r, e ) model -> ( model ++ ";" ++ r, List.map (Task.attempt (\r_ -> ( Debug.toString r_, [] ))) e )
        , view = \_ -> Html.text ""
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


all : Test
all =
    describe "simulated time effects"
        [ test "simulates Process.sleep" <|
            \() ->
                startTask [ Process.sleep 700 ]
                    |> TestContext.advanceTime 700
                    |> TestContext.expectModel (Expect.equal ";Ok ()")
        , test "sleep does not trigger until the delay has passed" <|
            \() ->
                startTask [ Process.sleep 700 ]
                    |> TestContext.advanceTime 699
                    |> TestContext.update ( "*", [] )
                    |> TestContext.advanceTime 1
                    |> TestContext.expectModel (Expect.equal ";*;Ok ()")
        , test "can chain multiple sleeps" <|
            \() ->
                startTask [ Process.sleep 250 |> Task.andThen (\() -> Process.sleep 25) ]
                    |> TestContext.advanceTime 274
                    |> TestContext.update ( "*", [] )
                    |> TestContext.advanceTime 1
                    |> TestContext.expectModel (Expect.equal ";*;Ok ()")
        , test "resolves sleeps in chronological order" <|
            \() ->
                startTask
                    [ Task.map (\() -> 10) (Process.sleep 10)
                    , Task.map (\() -> 900) (Process.sleep 900)
                    , Task.map (\() -> 33) (Process.sleep 33)
                    ]
                    |> TestContext.advanceTime 1000
                    |> TestContext.expectModel (Expect.equal ";Ok 10;Ok 33;Ok 900")
        , test "non-future events are immediately triggered" <|
            \() ->
                startTask [ Process.sleep 0 ]
                    |> TestContext.expectModel (Expect.equal ";Ok ()")
        , test "sleeps queued after time has advanced are queued at the correct time" <|
            \() ->
                startTask []
                    |> TestContext.advanceTime 100
                    |> TestContext.update ( "*", [ Process.sleep 10 ] )
                    |> TestContext.advanceTime 9
                    |> TestContext.update ( "*", [] )
                    |> TestContext.advanceTime 1
                    |> TestContext.expectModel (Expect.equal ";*;*;Ok ()")
        ]
