module TestContextTests.SimulatedEffects.BasicTest exposing (all)

import Expect
import SimulatedEffect.Task as Task
import Test exposing (..)
import TestContext exposing (SimulatedEffect, SimulatedTask, TestContext)
import TestingProgram exposing (Msg(..))


startTask : SimulatedTask x a -> TestingProgram.TestContext
startTask initialTask =
    TestingProgram.startEffects
        (Task.attempt (Debug.toString >> Log) initialTask)


all : Test
all =
    describe "basic simulated effects"
        [ test "simulates Task.succeed" <|
            \() ->
                startTask (Task.succeed ())
                    |> TestContext.expectModel (Expect.equal [ "Ok ()" ])
        , test "simulates Task.fail" <|
            \() ->
                startTask (Task.fail ())
                    |> TestContext.expectModel (Expect.equal [ "Err ()" ])
        , test "simulated Task.andThen" <|
            \() ->
                startTask (Task.succeed 5 |> Task.andThen ((+) 10 >> Task.fail))
                    |> TestContext.expectModel (Expect.equal [ "Err 15" ])
        ]
