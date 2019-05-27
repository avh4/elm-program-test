module TestContextTests.UserInput.FillInTest exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onClick)
import Test exposing (..)
import TestContext exposing (TestContext)


type TestEffect
    = NoOp
    | LogUpdate String


testInit : ( String, TestEffect )
testInit =
    ( "<INIT>"
    , NoOp
    )


testUpdate : String -> String -> ( String, TestEffect )
testUpdate msg model =
    ( model ++ ";" ++ msg
    , LogUpdate msg
    )


handleInput : String -> Html.Attribute String
handleInput fieldId =
    Html.Events.onInput (\text -> "Input:" ++ fieldId ++ ":" ++ text)


testView : String -> Html String
testView model =
    Html.div []
        [ Html.textarea [ handleInput "textarea" ] []
        , Html.div []
            [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
            , Html.input [ id "field-1", handleInput "field-1" ] []
            , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
            , Html.input [ id "field-2", handleInput "field-2" ] []
            ]
        , Html.div []
            [ Html.div [ id "button-a" ]
                [ Html.button [ onClick "CLICK-A" ] [ Html.text "Ambiguous click" ]
                ]
            , Html.div [ id "button-b" ]
                [ Html.button [ onClick "CLICK-B" ] [ Html.text "Ambiguous click" ]
                ]
            ]
        ]


testContext : TestContext String String TestEffect
testContext =
    TestContext.create
        { init = testInit
        , update = testUpdate
        , view = testView
        }


all : Test
all =
    describe "TestContext.fillIn (and fillInTextArea)"
        [ test "can simulate textarea input" <|
            \() ->
                testContext
                    |> TestContext.fillInTextarea "ABC"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:textarea:ABC")
        , test "can simulate text input on a labeled field" <|
            \() ->
                testContext
                    |> TestContext.fillIn "field-1" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can simulate text input on a labeled textarea" <|
            \() ->
                TestContext.create
                    { init = testInit
                    , update = testUpdate
                    , view =
                        \_ ->
                            Html.div []
                                [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
                                , Html.textarea [ id "field-1", handleInput "field-1" ] []
                                , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
                                , Html.textarea [ id "field-2", handleInput "field-2" ] []
                                ]
                    }
                    |> TestContext.fillIn "field-1" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        ]
