module TestContextTests.UserInput.ClickButtonTest exposing (all)

import Expect
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import Test exposing (..)
import TestContext
import TestingProgram exposing (Msg(..))


all : Test
all =
    describe "clicking buttons"
        [ test "can click a button" <|
            \() ->
                TestingProgram.startView
                    (Html.button
                        [ onClick (Log "CLICK") ]
                        [ Html.text "Click Me" ]
                    )
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectModel (Expect.equal [ "CLICK" ])
        , test "can click an elm-ui button" <|
            \() ->
                TestingProgram.startView
                    (Html.div
                        [ onClick (Log "CLICK")
                        , Html.Attributes.attribute "role" "button"
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectModel (Expect.equal [ "CLICK" ])
        ]
