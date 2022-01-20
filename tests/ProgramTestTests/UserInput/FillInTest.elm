module ProgramTestTests.UserInput.FillInTest exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (attribute, for, id)
import Html.Events
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Expect exposing (expectFailure)


handleInput : String -> Html.Attribute String
handleInput fieldId =
    Html.Events.onInput (\text -> "Input:" ++ fieldId ++ ":" ++ text)


start : List (Html String) -> ProgramTest String String ()
start view =
    ProgramTest.createSandbox
        { init = "<INIT>"
        , update = \msg model -> model ++ ";" ++ msg
        , view = \_ -> Html.node "body" [] view
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "ProgramTest.fillIn (and fillInTextArea)"
        [ test "can simulate textarea input" <|
            \() ->
                start
                    [ Html.textarea [ handleInput "textarea" ] []
                    ]
                    |> ProgramTest.fillInTextarea "ABC"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:textarea:ABC")
        , test "can simulate text input on a labeled field" <|
            \() ->
                start
                    [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
                    , Html.input [ id "field-1", handleInput "field-1" ] []
                    , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
                    , Html.input [ id "field-2", handleInput "field-2" ] []
                    ]
                    |> ProgramTest.fillIn "field-1" "Field 1" "value99"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can simulate text input on a labeled textarea" <|
            \() ->
                start
                    [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
                    , Html.textarea [ id "field-1", handleInput "field-1" ] []
                    , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
                    , Html.textarea [ id "field-2", handleInput "field-2" ] []
                    ]
                    |> ProgramTest.fillIn "field-1" "Field 1" "value99"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can find input contained in the label" <|
            \() ->
                start
                    [ Html.label []
                        [ Html.div [] [ Html.text "Field 1" ]
                        , Html.input [ handleInput "field-1" ] []
                        ]
                    ]
                    |> ProgramTest.fillIn "" "Field 1" "value99"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can find input with hidden label" <|
            \() ->
                start
                    [ Html.input
                        [ handleInput "field-1"
                        , attribute "aria-label" "Field 1"
                        ]
                        []
                    ]
                    |> ProgramTest.fillIn "" "Field 1" "value99"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "can find input with aria-label and an id" <|
            \() ->
                start
                    [ Html.input
                        [ handleInput "field-1"
                        , attribute "aria-label" "Field 1"
                        , id "field-id"
                        ]
                        []
                    ]
                    |> ProgramTest.fillIn "field-id" "Field 1" "value99"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Input:field-1:value99")
        , test "error message with labelled input" <|
            \() ->
                start
                    [ Html.label
                        [ for "field-1"
                        ]
                        [ Html.text "Field 1"
                        ]
                    ]
                    |> ProgramTest.fillIn "field-1" "Field 1" "value99"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <label htmlFor=\"field-1\">"
                        , "            Field 1"
                        , "        </label>"
                        , "    </body>"
                        , ""
                        , ""
                        , """▼ ProgramTest.fillIn "Field 1\""""
                        , ""
                        , "Expected one of the following to exist and have an \"oninput\" handler:"
                        , """- <label for="field-1"> with text "Field 1" and an <input id="field-1">"""
                        , """    \u{001B}[32m✓\u{001B}[39m label exists:"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas tag "label"\u{001B}[22m"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas attribute "htmlFor" "field-1"\u{001B}[22m"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas text "Field 1"\u{001B}[22m"""
                        , """    \u{001B}[31m✗ has tag "input"\u{001B}[39m"""
                        , """- <label for="field-1"> with text "Field 1" and a <textarea id="field-1">"""
                        , """    \u{001B}[32m✓\u{001B}[39m label exists:"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas tag "label"\u{001B}[22m"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas attribute "htmlFor" "field-1"\u{001B}[22m"""
                        , """      \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas text "Field 1"\u{001B}[22m"""
                        , """    \u{001B}[31m✗ has tag "textarea"\u{001B}[39m"""
                        ]
        , test "error message for when an input is in a label" <|
            \() ->
                start
                    [ Html.label []
                        [ Html.text "Field 1"
                        , Html.b [] []
                        ]
                    ]
                    |> ProgramTest.fillIn "" "Field 1" "value99"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <label>"
                        , "            Field 1"
                        , "            <b>"
                        , "            </b>"
                        , "        </label>"
                        , "    </body>"
                        , ""
                        , ""
                        , """▼ ProgramTest.fillIn "Field 1\""""
                        , ""
                        , "Expected one of the following to exist and have an \"oninput\" handler:"
                        , """- a <label> with text "Field 1" containing an <input>"""
                        , """    \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas tag "label"\u{001B}[22m"""
                        , """    \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas containing [ text "Field 1" ]\u{001B}[22m"""
                        , """      \u{001B}[31m✗ has tag "input"\u{001B}[39m"""
                        , """- a <label> with text "Field 1" containing a <textarea>"""
                        , """    \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas tag "label"\u{001B}[22m"""
                        , """    \u{001B}[32m✓\u{001B}[39m \u{001B}[1mhas containing [ text "Field 1" ]\u{001B}[22m"""
                        , """      \u{001B}[31m✗ has tag "textarea"\u{001B}[39m"""
                        ]
        , test "shows a useful error when the input doesn't exist" <|
            \() ->
                start [ Html.text "no input" ]
                    |> ProgramTest.fillIn "field-id" "Field 1" "value99"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        no input"
                        , "    </body>"
                        , ""
                        , ""
                        , """▼ ProgramTest.fillIn "Field 1\""""
                        , ""
                        , "Expected one of the following to exist and have an \"oninput\" handler:"
                        , """- <label for="field-id"> with text "Field 1" and an <input id="field-id">"""
                        , """    \u{001B}[31m✗ has tag "label"\u{001B}[39m"""
                        , """- <input aria-label="Field 1" id="field-id">"""
                        , """    \u{001B}[31m✗ has tag "input"\u{001B}[39m"""
                        , """- <label for="field-id"> with text "Field 1" and a <textarea id="field-id">"""
                        , """    \u{001B}[31m✗ has tag "label"\u{001B}[39m"""
                        , """- <textarea aria-label="Field 1" id="field-id">"""
                        , """    \u{001B}[31m✗ has tag "textarea"\u{001B}[39m"""
                        ]
        ]
