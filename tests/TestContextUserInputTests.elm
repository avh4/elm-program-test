module TestContextUserInputTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (for, id, value)
import Html.Events exposing (on)
import Json.Decode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import TestContext exposing (TestContext)


testContext : TestContext String String ()
testContext =
    TestContext.create
        { init = testInit
        , update = testUpdate
        , view = testView
        }


testInit : ( String, () )
testInit =
    ( "<INIT>"
    , ()
    )


testUpdate : String -> String -> ( String, () )
testUpdate msg model =
    ( model ++ ";" ++ msg
    , ()
    )


testView : String -> Html String
testView model =
    Html.div []
        [ Html.label [ for "pet-select" ] [ Html.text "Choose a pet" ]
        , Html.select
            [ id "pet-select"
            , on "change" Html.Events.targetValue
            ]
            [ Html.option [ value "dog-value" ] [ Html.text "Dog" ]
            , Html.option [ value "cat-value" ] [ Html.text "Cat" ]
            , Html.option [ value "hamster-value" ] [ Html.text "Hamster" ]
            ]
        ]


all : Test
all =
    describe "simulating user input"
        [ describe "selectOption"
            [ test "can select an option" <|
                \() ->
                    testContext
                        |> TestContext.selectOption "pet-select" "Choose a pet" "hamster-value" "Hamster"
                        |> TestContext.expectModel (Expect.equal "<INIT>;hamster-value")
            ]
        ]
