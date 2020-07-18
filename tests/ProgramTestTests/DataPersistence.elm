module ProgramTestTests.DataPersistence exposing (all)

import Expect
import Html
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Ports as Ports
import Test exposing (..)
import Test.Expect exposing (expectFailure, expectFailureContaining)
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "data persistence"
        [ test "can retrieve a value from outgoing port" <|
            \() ->
                TestingProgram.startEffects (Ports.send "myPort" (Encode.string "Foo"))
                    |> ProgramTest.getOutgoingPortValues "myPort"
                    |> Result.map (List.map (Encode.encode 0))
                    |> Expect.equal (Ok [ "\"Foo\"" ])
        , test "getPortValues but program has already errored" <|
            \() ->
                TestingProgram.startEffects (Ports.send "myPort" (Encode.string "Foo"))
                    |> ProgramTest.clickButton "Not found button"
                    |> ProgramTest.getOutgoingPortValues "myPort"
                    |> Result.mapError ProgramTest.done
                    |> Result.map (\_ -> Expect.pass)
                    |> joinResult
                    |> expectFailureContaining "Not found button"
        , test "when effect simulation isn't configured" <|
            \() ->
                ProgramTest.createElement
                    { init = \() -> ( [], () )
                    , update = \() _ -> ( [], () )
                    , view = \_ -> Html.node "body" [] []
                    }
                    |> ProgramTest.start ()
                    |> ProgramTest.getOutgoingPortValues "myPort"
                    |> Result.mapError ProgramTest.done
                    |> Result.map (\_ -> Expect.pass)
                    |> joinResult
                    |> expectFailure
                        [ "TEST SETUP ERROR: In order to use getPortValues, you MUST use ProgramTest.withSimulatedEffects before calling ProgramTest.start"
                        ]
        ]


joinResult : Result a a -> a
joinResult result =
    case result of
        Err a ->
            a

        Ok a ->
            a
