module HomeAutomationExampleTest exposing (all)

import Expect
import HomeAutomationExample as Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)


start : ProgramTest Main.Model Main.Msg Main.Effect
start =
    ProgramTest.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start ()


all : Test
all =
    describe "HomeAutomationExample"
        [ test "controlling a light" <|
            \() ->
                start
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        "http://localhost:8003/lighting_service/v1/devices"
                        """[{"id":"K001", "name":"Kitchen", "dimmable":false, "value":0}]"""
                    |> ProgramTest.clickButton "Turn on"
                    |> ProgramTest.expectHttpRequest
                        "POST"
                        "http://localhost:8003/lighting_service/v1/devices/K001"
                        (.body >> Expect.equal """{"value":1}""")
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.GetDeviceList { url, onResult, decoder } ->
            SimulatedEffect.Http.get
                { url = url
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }

        Main.ChangeLight { url, onResult, decoder, body } ->
            SimulatedEffect.Http.post
                { url = url
                , body = SimulatedEffect.Http.jsonBody body
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }
