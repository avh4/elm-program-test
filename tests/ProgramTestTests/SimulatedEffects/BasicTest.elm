module ProgramTestTests.SimulatedEffects.BasicTest exposing (all)

import Expect
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Task as Task
import Test exposing (..)
import TestingProgram exposing (Msg(..))


startTask : SimulatedTask x a -> TestingProgram.ProgramTest
startTask initialTask =
    TestingProgram.startEffects
        (Task.attempt (Debug.toString >> Log) initialTask)


all : Test
all =
    describe "basic simulated effects"
        [ test "simulates Task.succeed" <|
            \() ->
                startTask (Task.succeed ())
                    |> ProgramTest.expectModel (Expect.equal [ "Ok ()" ])
        , test "simulates Task.fail" <|
            \() ->
                startTask (Task.fail ())
                    |> ProgramTest.expectModel (Expect.equal [ "Err ()" ])
        , test "simulated Task.andThen" <|
            \() ->
                startTask (Task.succeed 5 |> Task.andThen ((+) 10 >> Task.fail))
                    |> ProgramTest.expectModel (Expect.equal [ "Err 15" ])
        , test "simulated Task.sequence" <|
            \() ->
                startTask (Task.sequence [ Task.succeed 1, Task.succeed 2 ])
                    |> ProgramTest.expectModel (Expect.equal [ "Ok [1,2]" ])
        , test "simulated Task.sequence fail" <|
            \() ->
                startTask (Task.sequence [ Task.succeed 1, Task.fail () ])
                    |> ProgramTest.expectModel (Expect.equal [ "Err ()" ])
        ]
