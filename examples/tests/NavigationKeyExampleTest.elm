module NavigationKeyExampleTest exposing (all)

import NavigationKeyExample as Main exposing (Model, Msg)
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)


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

        -- TODO: elm-program-test does not yet intercept link clicks when using Browser.application
        -- TODO: see <https://github.com/avh4/elm-program-test/issues/107>
        --, test "example of clicking an a tag link" <|
        --    \() ->
        --        start
        --            |> within (Query.find [ tag "header" ])
        --                (clickLink "User 2" "/users/2")
        --            |> within (Query.find [ tag "main" ])
        --                (ensureViewHas [ text "User 2" ])
        --            |> done
        ]
