module ElementWithNavigationExampleTest exposing (all)

import ElementWithNavigationExample as Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Test exposing (..)


start : String -> ProgramTest Main.Msg Main.Model Main.Effect
start initialUrl =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start initialUrl


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.PushUrlEffect url ->
            SimulatedEffect.Navigation.pushUrl url


all : Test
all =
    describe "ElementWithNavigationExample"
        [ test "navigating between pages" <|
            \() ->
                start "/"
                    |> ProgramTest.clickLink "Post #7" "/blog/7"
                    |> ProgramTest.clickLink "Home" "/"
                    |> ProgramTest.done
        ]
