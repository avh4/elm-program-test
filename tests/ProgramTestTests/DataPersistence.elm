module ProgramTestTests.DataPersistence exposing (all)

import Expect
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedTask)
import SimulatedEffect.Ports as Ports
import Test exposing (..)
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "data persistence"
        [ test "can retrieve a value from outgoing port" <|
            \() ->
                TestingProgram.startEffects (Ports.send "myPort" (Encode.string "Foo"))
                    |> ProgramTest.getPortValues "myPort"
                    |> List.map (Encode.encode 0)
                    |> Expect.equal [ "\"Foo\"" ]
        , todo "getPortValues but program has already errored"
        , todo "when effect simulation isn't configured"

        -- TODO: Is this needed?
        , todo "no values for port?"
        ]
