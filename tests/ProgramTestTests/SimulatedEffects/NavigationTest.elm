module ProgramTestTests.SimulatedEffects.NavigationTest exposing (all)

import Expect
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Test exposing (..)
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "simulating browser navigation"
        [ test "can simulate a route change" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.routeChange "https://example.com/new"
                    |> ProgramTest.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "can simulate a route change with a relative URL" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.routeChange "/new"
                    |> ProgramTest.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "simulating a pushUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "new"))
                    |> ProgramTest.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "simulating a replaceUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.replaceUrl "/new"))
                    |> ProgramTest.expectModel (Expect.equal [ "https://example.com/new" ])
        ]
