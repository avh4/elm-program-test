module ProgramTestTests.SimulatedEffects.TimeTest exposing (all)

import Expect
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Cmd
import SimulatedEffect.Process as Process
import SimulatedEffect.Task as Task
import Test exposing (..)
import TestingProgram exposing (Msg(..))


startTasks : List (SimulatedTask x a) -> TestingProgram.ProgramTest
startTasks initialTasks =
    TestingProgram.startEffects
        (SimulatedEffect.Cmd.batch
            (List.map (Task.attempt (Debug.toString >> Log)) initialTasks)
        )


produceTasks : List (SimulatedTask x a) -> TestingProgram.Msg
produceTasks tasks =
    ProduceEffects
        (SimulatedEffect.Cmd.batch
            (List.map (Task.attempt (Debug.toString >> Log)) tasks)
        )


all : Test
all =
    describe "simulated time effects"
        [ test "simulates Process.sleep" <|
            \() ->
                startTasks [ Process.sleep 700 ]
                    |> ProgramTest.advanceTime 700
                    |> ProgramTest.expectModel (Expect.equal [ "Ok ()" ])
        , test "sleep does not trigger until the delay has passed" <|
            \() ->
                startTasks [ Process.sleep 700 ]
                    |> ProgramTest.advanceTime 699
                    |> ProgramTest.update (Log "*")
                    |> ProgramTest.advanceTime 1
                    |> ProgramTest.expectModel (Expect.equal [ "*", "Ok ()" ])
        , test "can chain multiple sleeps" <|
            \() ->
                startTasks [ Process.sleep 250 |> Task.andThen (\() -> Process.sleep 25) ]
                    |> ProgramTest.advanceTime 274
                    |> ProgramTest.update (Log "*")
                    |> ProgramTest.advanceTime 1
                    |> ProgramTest.expectModel (Expect.equal [ "*", "Ok ()" ])
        , test "resolves sleeps in chronological order" <|
            \() ->
                startTasks
                    [ Task.map (\() -> 10) (Process.sleep 10)
                    , Task.map (\() -> 900) (Process.sleep 900)
                    , Task.map (\() -> 33) (Process.sleep 33)
                    ]
                    |> ProgramTest.advanceTime 1000
                    |> ProgramTest.expectModel (Expect.equal [ "Ok 10", "Ok 33", "Ok 900" ])
        , test "non-future events are immediately triggered" <|
            \() ->
                startTasks [ Process.sleep 0 ]
                    |> ProgramTest.expectModel (Expect.equal [ "Ok ()" ])
        , test "sleeps queued after time has advanced are queued at the correct time" <|
            \() ->
                startTasks []
                    |> ProgramTest.advanceTime 100
                    |> ProgramTest.update (Log "A")
                    |> ProgramTest.update (produceTasks [ Process.sleep 10 ])
                    |> ProgramTest.advanceTime 9
                    |> ProgramTest.update (Log "B")
                    |> ProgramTest.advanceTime 1
                    |> ProgramTest.expectModel (Expect.equal [ "A", "B", "Ok ()" ])
        ]
