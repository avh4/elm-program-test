module TestContextTests.UserInput.FillInTest exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (attribute, for, id)
import Html.Events
import Test exposing (..)
import TestContext exposing (TestContext)


type TestEffect
    = NoOp
    | LogUpdate String


handleInput : String -> Html.Attribute String
handleInput fieldId =
    Html.Events.onInput (\text -> "Input:" ++ fieldId ++ ":" ++ text)


start : List (Html String) -> TestContext String String TestEffect
start view =
    TestContext.create
        { init = ( "<INIT>", NoOp )
        , update = \msg model -> ( model ++ ";" ++ msg, LogUpdate msg )
        , view = \_ -> Html.node "body" [] view
        }


all : Test
all =
    describe "TestContext.fillIn (and fillInTextArea)"
        [ test "can simulate textarea input" <|
            \() ->
                start
                    [ Html.textarea [ handleInput "textarea" ] []
                    ]
                    |> TestContext.fillInTextarea "ABC"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:textarea:ABC")
        , test "can simulate text input on a labeled field" <|
            \() ->
                start
                    [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
                    , Html.input [ id "field-1", handleInput "field-1" ] []
                    , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
                    , Html.input [ id "field-2", handleInput "field-2" ] []
                    ]
                    |> TestContext.fillIn "field-1" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can simulate text input on a labeled textarea" <|
            \() ->
                start
                    [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
                    , Html.textarea [ id "field-1", handleInput "field-1" ] []
                    , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
                    , Html.textarea [ id "field-2", handleInput "field-2" ] []
                    ]
                    |> TestContext.fillIn "field-1" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can find input contained in the label" <|
            \() ->
                start
                    [ Html.label []
                        [ Html.div [] [ Html.text "Field 1" ]
                        , Html.input [ handleInput "field-1" ] []
                        ]
                    ]
                    |> TestContext.fillIn "" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can find input with hidden label" <|
            \() ->
                start
                    [ Html.input
                        [ handleInput "field-1"
                        , attribute "aria-label" "Field 1"
                        ]
                        []
                    ]
                    |> TestContext.fillIn "" "Field 1" "value99"
                    |> TestContext.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        ]
