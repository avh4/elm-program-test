module TestContextTests.SimulatedEffects.NavigationTest exposing (all)

import Expect
import SimulatedEffect.Navigation
import Test exposing (..)
import TestContext
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "simulating browser navigation"
        [ test "can simulate a route change" <|
            \() ->
                TestingProgram.application []
                    |> TestContext.routeChange "https://example.com/new"
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "can simulate a route change with a relative URL" <|
            \() ->
                TestingProgram.application []
                    |> TestContext.routeChange "/new"
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        , test "simulating a pushUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application []
                    |> TestContext.update (ProduceEffects [ SimulatedEffect.Navigation.pushUrl "new" ])
                    |> TestContext.expectModel (Expect.equal [ "https://example.com/new" ])
        ]
