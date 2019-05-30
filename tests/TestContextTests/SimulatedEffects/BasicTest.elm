module TestContextTests.SimulatedEffects.BasicTest exposing (all)

import Expect
import Html
import SimulatedEffect.Task as Task
import Test exposing (..)
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)


startTask : SimulatedTask x a -> TestContext (Result x a) String (List (SimulatedEffect (Result x a)))
startTask initialTask =
    TestContext.createElement
        { init = \() -> ( "", [ Task.attempt identity initialTask ] )
        , update = \msg model -> ( model ++ ";" ++ Debug.toString msg, [] )
        , view = \_ -> Html.text ""
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


all : Test
all =
    describe "basic simulated effects"
        [ test "simulates Task.succeed" <|
            \() ->
                startTask (Task.succeed ())
                    |> TestContext.expectModel (Expect.equal ";Ok ()")
        , test "simulates Task.fail" <|
            \() ->
                startTask (Task.fail ())
                    |> TestContext.expectModel (Expect.equal ";Err ()")
        , test "simulated Task.andThen" <|
            \() ->
                startTask (Task.succeed 5 |> Task.andThen ((+) 10 >> Task.fail))
                    |> TestContext.expectModel (Expect.equal ";Err 15")
        ]
