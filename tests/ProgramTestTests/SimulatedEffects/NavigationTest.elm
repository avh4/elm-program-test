module ProgramTestTests.SimulatedEffects.NavigationTest exposing (all)

import Expect
import Html
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "simulating browser navigation"
        [ test "can simulate a route change" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.routeChange "https://example.com/new"
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/new" ])
        , test "can simulate a route change with a relative URL" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.routeChange "/new"
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/new" ])
        , test "simulating a pushUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "new"))
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/new" ])
        , test "simulating a pushUrl changes the browser URL" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "new"))
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/new")
        , test "simulating a replaceUrl triggers an onUrlChange" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.replaceUrl "/new"))
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/new" ])
        , test "with a base url, expectUrl returns the base url" <|
            \() ->
                ProgramTest.createElement
                    { init = \() -> ( (), () )
                    , update = \msg () -> ( (), () )
                    , view = \_ -> Html.text ""
                    }
                    |> ProgramTest.withBaseUrl "https://example.com/start"
                    |> ProgramTest.start ()
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/start")
        , test "routeChange changes browser URL" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.routeChange "https://example.com/new"
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/new")
        , test "browser history is intially empty" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.expectBrowserHistory (Expect.equal [])
        , test "simulating pushUrl adds to the browser history" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.expectBrowserHistory (Expect.equal [ "https://example.com/path" ])
        , test "simulating replaceUrl does NOT add to the browser history" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.replaceUrl "https://example.com/new")
                    |> ProgramTest.expectBrowserHistory (Expect.equal [])
        , test "simulating back goes to the previous URL" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 1))
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/path")
        , test "simulating back updates the browser history" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "https://example.com/new2"))
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 1))
                    |> ProgramTest.expectBrowserHistory (Expect.equal [ "https://example.com/path" ])
        , test "simulating back produces onUrlChange" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update Clear
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 1))
                    -- TODO: real elm seems to produce 2 OnUrlChanges, the old and new URL - is that intentional?
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/path" ])
        , test "simulating back bigger than history does nothing" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 2))
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/new")
        , test "simulating back more than one" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.pushUrl "https://example.com/new2"))
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 2))
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/path")
        , test "simulating back zero" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.pushUrl "https://example.com/new")
                    |> ProgramTest.update (ProduceEffects (SimulatedEffect.Navigation.back 0))
                    |> ProgramTest.expectBrowserUrl (Expect.equal "https://example.com/new")
        , test "simulate loading a page" <|
            \() ->
                TestingProgram.application (SimulatedEffect.Navigation.load "https://example.com")
                    |> ProgramTest.expectPageChange "https://example.com/"
        , test "simulate reloading a page" <|
            \() ->
                TestingProgram.application SimulatedEffect.Navigation.reload
                    |> ProgramTest.expectPageChange "https://example.com/path"
        , test "simulate reloading a page with skipped cache" <|
            \() ->
                TestingProgram.application SimulatedEffect.Navigation.reloadAndSkipCache
                    |> ProgramTest.expectPageChange "https://example.com/path"
        , test "expecting a page change without simulating one results in failure" <|
            \() ->
                TestingProgram.application SimulatedEffect.Cmd.none
                    |> ProgramTest.expectPageChange "https://example.com/"
                    |> expectFailure [ "expectPageChange: expected to have navigated to a different URL, but no links were clicked and no browser navigation was simulated" ]
        ]
