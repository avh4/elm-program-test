module ProgramTestTests.SimulatedEffects.TimeTest exposing (all)

import Expect
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Cmd
import SimulatedEffect.Process as Process
import SimulatedEffect.Task as Task
import SimulatedEffect.Time as Time
import Test exposing (..)
import TestingProgram exposing (Msg(..))
import Time as RealTime


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
        , describe "Time.now"
            [ test "time is initially Posix 0" <|
                \() ->
                    startTasks [ Time.now ]
                        |> ProgramTest.expectModel (Expect.equal [ "Ok (Posix 0)" ])
            , test "advanceTime is reflected" <|
                \() ->
                    startTasks []
                        |> ProgramTest.advanceTime 134
                        |> ProgramTest.update (produceTasks [ Time.now ])
                        |> ProgramTest.expectModel (Expect.equal [ "Ok (Posix 134)" ])
            , test "it resolves to the time at the moment it resolves" <|
                \() ->
                    startTasks [ Process.sleep 17 |> Task.andThen (\() -> Time.now) ]
                        |> ProgramTest.advanceTime 2000
                        |> ProgramTest.expectModel (Expect.equal [ "Ok (Posix 17)" ])
            ]
        ]
