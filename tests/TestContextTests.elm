module TestContextTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Events exposing (onClick)
import Test exposing (..)
import TestContext


testInit : String
testInit =
    "<INIT>"


testUpdate : String -> String -> String
testUpdate msg model =
    model ++ ";" ++ msg


testView : String -> Html String
testView model =
    Html.div []
        [ Html.span [] [ Html.text model ]
        , Html.button [ onClick "CLICK" ] [ Html.text "Click Me" ]
        ]


all : Test
all =
    describe "TestContext"
        [ test "has initial model" <|
            \() ->
                TestContext.create
                    { init = testInit
                    }
                    |> TestContext.expectModel (Expect.equal "<INIT>")
                    |> TestContext.done
        ]
