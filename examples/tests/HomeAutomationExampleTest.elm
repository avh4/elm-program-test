module HomeAutomationExampleTest exposing (all)

import HomeAutomationExample as Main
import Test exposing (..)
import TestContext


all : Test
all =
    describe "HomeAutomationExample"
        [ test "controlling a light" <|
            \() ->
                TestContext.createDocument
                    { init = Main.init
                    , update = Main.update
                    , view = Main.view
                    }
                    |> TestContext.start ()
                    |> TestContext.done
        ]
