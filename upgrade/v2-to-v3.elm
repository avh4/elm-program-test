module Main exposing (upgrade_TestContext_createWithJsonStringFlags)

import ProgramTest
import SimulatedEffect.Http



-- Creating


type alias Upgrade_TestContext_TestContext msg model effect =
    ProgramTest.ProgramTest model msg effect


upgrade_TestContext_create config =
    ProgramTest.createElement
        { init = \() -> config.init
        , view = config.view
        , update = config.update
        }
        |> ProgramTest.start ()


upgrade_TestContext_createWithFlags config flags =
    ProgramTest.createElement
        { init = config.init
        , view = config.view
        , update = config.update
        }
        |> ProgramTest.start flags


upgrade_TestContext_createWithJsonStringFlags decoder config json =
    ProgramTest.createElement
        { init = config.init
        , view = config.view
        , update = config.update
        }
        |> ProgramTest.withJsonStringFlags decoder
        |> ProgramTest.start json


upgrade_TestContext_createWithBaseUrl config baseUrl =
    ProgramTest.createElement
        { init = \() -> config.init
        , view = config.view
        , update = config.update
        }
        |> ProgramTest.withBaseUrl baseUrl
        |> ProgramTest.start ()


upgrade_TestContext_createWithNavigation onUrlChange config baseUrl =
    ProgramTest.createApplication
        { init = \() initialUrl _ -> config.init initialUrl
        , view =
            \model ->
                { title = ""
                , body = [ config.view model ]
                }
        , update = config.update
        , onUrlRequest = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the onUrlRequest function used by your application"
        , onUrlChange = onUrlChange
        }
        |> ProgramTest.withBaseUrl baseUrl
        |> ProgramTest.start ()


upgrade_TestContext_createWithNavigationAndFlags onUrlChange config baseUrl flags =
    ProgramTest.createApplication
        { init = \initialFlags initialUrl _ -> config.init initialFlags initialUrl
        , view =
            \model ->
                { title = ""
                , body = [ config.view model ]
                }
        , update = config.update
        , onUrlRequest = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the onUrlRequest function used by your application"
        , onUrlChange = onUrlChange
        }
        |> ProgramTest.withBaseUrl baseUrl
        |> ProgramTest.start flags


upgrade_TestContext_createWithNavigationAndJsonStringFlags decoder onUrlChange config baseUrl flags =
    ProgramTest.createApplication
        { init = \initialFlags initialUrl _ -> config.init initialFlags initialUrl
        , view =
            \model ->
                { title = ""
                , body = [ config.view model ]
                }
        , update = config.update
        , onUrlRequest = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the onUrlRequest function used by your application"
        , onUrlChange = onUrlChange
        }
        |> ProgramTest.withBaseUrl baseUrl
        |> ProgramTest.withJsonStringFlags decoder
        |> ProgramTest.start flags



-- Simulated effects


upgrade_TestContext_HttpRequest request =
    case request.method of
        "GET" ->
            SimulatedEffect.Http.get
                { url = request.url
                , expect = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the request expect"
                }

        "POST" ->
            SimulatedEffect.Http.post
                { url = request.url
                , body = SimulatedEffect.Http.emptyBody -- TODO: elm-program-test 3.x upgrade: provide the actual request body
                , expect = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the request expect"
                }

        _ ->
            SimulatedEffect.Http.request
                { method = request.method
                , headers = []
                , url = request.url
                , body = SimulatedEffect.Http.emptyBody -- TODO: elm-program-test 3.x upgrade: provide the actual request body
                , expect = Debug.todo "TODO: elm-program-test 3.x upgrade: provide the request expect"
                , timeout = Nothing
                , tracker = Nothing
                }


upgrade_TestContext_createWithSimulatedEffects config =
    ProgramTest.createElement
        { init = \() -> config.init
        , view = config.view
        , update = config.update
        }
        |> ProgramTest.withSimulatedEffects config.deconstructEffect
        |> ProgramTest.start ()



-- Simulating user input


upgrade_TestContext_clickButton =
    ProgramTest.clickButton


upgrade_TestContext_clickLink =
    ProgramTest.clickLink


upgrade_TestContext_fillIn =
    ProgramTest.fillIn


upgrade_TestContext_fillInTextarea =
    ProgramTest.fillInTextarea


upgrade_TestContext_check =
    ProgramTest.check


upgrade_TestContext_selectOption =
    ProgramTest.selectOption


upgrade_TestContext_routeChange =
    ProgramTest.routeChange



-- Simulating user input (advanced)


upgrade_TestContext_simulate =
    ProgramTest.simulateDomEvent


upgrade_TestContext_within =
    ProgramTest.within



-- Simulating HTTP responses


upgrade_TestContext_assertHttpRequest request =
    ProgramTest.expectHttpRequestWasMade request.method request.url



-- Directly sending Msgs


upgrade_TestContext_update =
    ProgramTest.update


upgrade_TestContext_simulateLastEffect =
    -- TODO: elm-program-test 3.x upgrade:
    -- if you are simulating HTTP responses, you should prefer ProgramTest.withSimulatedEffects
    -- See https://elm-program-test.netlify.com/cmds.html for more details
    ProgramTest.simulateLastEffect



-- Final assertions


upgrade_TestContext_expectViewHas =
    ProgramTest.expectViewHas


upgrade_TestContext_expectView =
    ProgramTest.expectView


upgrade_TestContext_expectLastEffect =
    ProgramTest.expectLastEffect


upgrade_TestContext_expectModel =
    ProgramTest.expectModel


upgrade_TestContext_expectPageChange =
    ProgramTest.expectPageChange



-- Intermediate assertions


upgrade_TestContext_shouldHave =
    ProgramTest.ensureViewHas


upgrade_TestContext_shouldNotHave =
    ProgramTest.ensureViewHasNot


upgrade_TestContext_shouldHaveView =
    ProgramTest.ensureView


upgrade_TestContext_shouldHaveLastEffect =
    ProgramTest.ensureLastEffect


upgrade_TestContext_done =
    ProgramTest.done



-- Custom assertions


upgrade_TestContext_fail =
    ProgramTest.fail


upgrade_TestContext_createFailed =
    ProgramTest.createFailed
