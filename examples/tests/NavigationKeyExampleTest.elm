module NavigationKeyExampleTest exposing (all)

import Expect
import NavigationKeyExample as Main exposing (Effect(..), Model, Msg)
import ProgramTest exposing (..)
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import SimulatedEffect.Process
import SimulatedEffect.Task
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)


type alias ProgramTest =
    ProgramTest.ProgramTest (Model ()) Msg Effect


start : String -> ProgramTest
start initialRoute =
    ProgramTest.createApplication
        { init = Main.init
        , view = Main.view
        , update = Main.update
        , onUrlRequest = Main.OnUrlRequest
        , onUrlChange = Main.OnUrlChange
        }
        |> ProgramTest.withBaseUrl ("https://example.com" ++ initialRoute)
        |> ProgramTest.withSimulatedEffects simulateEffect
        |> ProgramTest.start ()


all : Test
all =
    describe "NavigationKeyExample"
        [ test "starts on the Home page" <|
            \() ->
                start "/"
                    |> expectViewHas [ text "Home" ]

        -- TODO: elm-program-test does not yet intercept link clicks when using Browser.application
        -- TODO: see <https://github.com/avh4/elm-program-test/issues/107>
        --, test "example of clicking an a tag link" <|
        --    \() ->
        --        start "/"
        --            |> within (Query.find [ tag "header" ])
        --                (clickLink "User 2" "/users/2")
        --            |> within (Query.find [ tag "main" ])
        --                (ensureViewHas [ text "User 2" ])
        --            |> done
        , test "example of navigating via an effect" <|
            \() ->
                start "/some-page"
                    |> clickButton "Go Home in 3 seconds"
                    |> ProgramTest.advanceTime 2999
                    |> ensureBrowserUrl (Expect.equal "https://example.com/some-page")
                    |> ProgramTest.advanceTime 1
                    |> ensureBrowserUrl (Expect.equal "https://example.com/")
                    |> within (Query.find [ tag "main" ])
                        (ensureViewHas [ text "Home" ])
                    |> done
        ]


simulateEffect : Effect -> SimulatedEffect Msg
simulateEffect effect =
    case effect of
        NoEffect ->
            SimulatedEffect.Cmd.none

        Delay ms msg ->
            SimulatedEffect.Process.sleep ms
                |> SimulatedEffect.Task.perform (\() -> msg)

        PushUrl string ->
            SimulatedEffect.Navigation.pushUrl string

        Load string ->
            SimulatedEffect.Navigation.load string
