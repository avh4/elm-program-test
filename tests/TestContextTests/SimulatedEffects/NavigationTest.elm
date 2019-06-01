module TestContextTests.SimulatedEffects.NavigationTest exposing (all)

import Expect
import Test exposing (..)
import TestContext
import TestingProgram


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
        ]
