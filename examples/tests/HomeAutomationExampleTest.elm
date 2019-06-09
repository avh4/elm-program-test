module HomeAutomationExampleTest exposing (all)

import Expect
import HomeAutomationExample as Main
import SimulatedEffect.Http
import Test exposing (..)
import TestContext exposing (TestContext)


start : TestContext Main.Msg Main.Model Main.Effect
start =
    TestContext.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> TestContext.withSimulatedEffects simulateEffects
        |> TestContext.start ()


all : Test
all =
    describe "HomeAutomationExample"
        [ test "controlling a light" <|
            \() ->
                start
                    |> TestContext.simulateHttpOk
                        "GET"
                        "http://localhost:8003/lighting_service/v1/devices"
                        """[{"id":"K001", "name":"Kitchen", "dimmable":false, "value":0}]"""
                    |> TestContext.clickButton "Turn on"
                    |> TestContext.assertHttpRequest
                        "POST"
                        "http://localhost:8003/lighting_service/v1/devices/K001"
                        (.body >> Expect.equal """{"value":1}""")
                    |> TestContext.done
        ]


simulateEffects : Main.Effect -> List (TestContext.SimulatedEffect Main.Msg)
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            []

        Main.GetDeviceList { url, onResult, decoder } ->
            [ SimulatedEffect.Http.get
                { url = url
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }
            ]

        Main.ChangeLight { url, onResult, decoder, body } ->
            [ SimulatedEffect.Http.post
                { url = url
                , body = SimulatedEffect.Http.jsonBody body
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }
            ]
