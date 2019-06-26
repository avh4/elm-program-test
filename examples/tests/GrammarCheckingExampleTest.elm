module GrammarCheckingExampleTest exposing (all)

import Expect
import GrammarCheckingExample as Main
import Json.Decode
import Json.Encode
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import Test exposing (..)
import Test.Html.Selector exposing (text)
import TestContext exposing (TestContext)


start : TestContext Main.Msg Main.Model Main.Effect
start =
    TestContext.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> TestContext.withSimulatedEffects simulateEffects
        |> TestContext.withSimulatedSubscriptions simulateSub
        |> TestContext.start ()


all : Test
all =
    describe "GrammarCheckingExample"
        [ test "checking grammar" <|
            \() ->
                start
                    |> TestContext.fillIn "main"
                        "Enter text to check"
                        "The youngest man the boat."
                    |> TestContext.clickButton "Check"
                    |> TestContext.assertAndClearOutgoingPortValues
                        "checkGrammar"
                        Json.Decode.string
                        (Expect.equal [ "The youngest man the boat." ])
                    |> TestContext.simulateIncomingPort
                        "grammarCheckResults"
                        (Json.Encode.list Json.Encode.string
                            [ "Garden-path sentences can confuse the reader." ]
                        )
                    |> TestContext.expectViewHas
                        [ text "Garden-path sentences can confuse the reader." ]
        ]


simulateEffects : Main.Effect -> TestContext.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.CheckGrammarEffect text ->
            SimulatedEffect.Ports.send "checkGrammar" (Json.Encode.string text)


simulateSub : Main.Model -> TestContext.SimulatedSub Main.Msg
simulateSub _ =
    SimulatedEffect.Ports.subscribe "grammarCheckResults"
        (Json.Decode.list Json.Decode.string)
        Main.GrammarCheckResults
