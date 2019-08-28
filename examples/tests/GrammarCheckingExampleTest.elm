module GrammarCheckingExampleTest exposing (all)

import Expect
import GrammarCheckingExample as Main
import Json.Decode
import Json.Encode
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import Test exposing (..)
import Test.Html.Selector exposing (text)


start : ProgramTest Main.Model Main.Msg Main.Effect
start =
    ProgramTest.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.withSimulatedSubscriptions simulateSub
        |> ProgramTest.start ()


all : Test
all =
    describe "GrammarCheckingExample"
        [ test "checking grammar" <|
            \() ->
                start
                    |> ProgramTest.fillIn "main"
                        "Enter text to check"
                        "The youngest man the boat."
                    |> ProgramTest.clickButton "Check"
                    |> ProgramTest.ensureOutgoingPortValues
                        "checkGrammar"
                        Json.Decode.string
                        (Expect.equal [ "The youngest man the boat." ])
                    |> ProgramTest.simulateIncomingPort
                        "grammarCheckResults"
                        (Json.Encode.list Json.Encode.string
                            [ "Garden-path sentences can confuse the reader." ]
                        )
                    |> ProgramTest.expectViewHas
                        [ text "Garden-path sentences can confuse the reader." ]
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.CheckGrammarEffect text ->
            SimulatedEffect.Ports.send "checkGrammar" (Json.Encode.string text)


simulateSub : Main.Model -> ProgramTest.SimulatedSub Main.Msg
simulateSub _ =
    SimulatedEffect.Ports.subscribe "grammarCheckResults"
        (Json.Decode.list Json.Decode.string)
        Main.GrammarCheckResults
