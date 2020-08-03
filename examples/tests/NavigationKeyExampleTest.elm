module NavigationKeyExampleTest exposing (all)

import NavigationKeyExample as Main exposing (Model, Msg)
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Selector exposing (text)


type alias ProgramTest =
    ProgramTest.ProgramTest (Model ()) Msg (Cmd Msg)


start : ProgramTest
start =
    ProgramTest.createApplication
        { init = Main.init
        , view = Main.view
        , update = Main.update
        , onUrlRequest = Main.OnUrlRequest
        , onUrlChange = Main.OnUrlChange
        }
        |> ProgramTest.withBaseUrl "https://example.com/"
        |> ProgramTest.start ()


all : Test
all =
    describe "NavigationKeyExample"
        [ test "starts on the Home page" <|
            \() ->
                start
                    |> expectViewHas [ text "Home" ]
        ]
