module TestContextTests.SimulatedEffects.NavigationTest exposing (all)

import Expect
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Test exposing (..)
import TestContext
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "simulating browser navigation"
        [ test "can simulate a route change" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> TestContext.routeChange "https://example.com/new"
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "can simulate a route change with a relative URL" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> TestContext.routeChange "/new"
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "simulating a pushUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> TestContext.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "new"))
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "simulating a replaceUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> TestContext.update (ProduceEffects (SimulatedEffect.Navigation.replaceUrl "/new"))
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        ]
