module TestContextTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Events exposing (onClick)
import Test exposing (..)
import TestContext exposing (TestContext)


testInit : String
testInit =
    "<INIT>"


testUpdate : String -> String -> ( String, Cmd msg )
testUpdate msg model =
    ( model ++ ";" ++ msg
    , Cmd.none
    )


testView : String -> Html String
testView model =
    Html.div []
        [ Html.span [] [ Html.text model ]
        , Html.button [ onClick "CLICK" ] [ Html.text "Click Me" ]
        ]


testContext : TestContext String String
testContext =
    TestContext.create
        { init = testInit
        , update = testUpdate
        }


all : Test
all =
    describe "TestContext"
        [ test "has initial model" <|
            \() ->
                testContext
                    |> TestContext.expectModel (Expect.equal "<INIT>")
                    |> TestContext.done
        , test "can send a msg" <|
            \() ->
                testContext
                    |> TestContext.update "A"
                    |> TestContext.expectModel (Expect.equal "<INIT>;A")
                    |> TestContext.done
        ]
