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
                        , "        <label for=\"field-1\">"
                        , "            Field 1"
                        , "        </label>"
                        , "    </body>"
                        , ""
                        , ""
                        , """▼ ProgramTest.fillIn "Field 1\""""
                        , ""
                        , "Expected one of the following to exist and have an \"oninput\" handler:"
                        , """- <input> associated to <label> by id"""
                        , """    ✓ check label exists:"""
                        , """      ✓ has tag "label\""""
                        , """      ✓ has attribute "htmlFor" "field-1\""""
                        , """      ✓ has text "Field 1\""""
                        , """    ✗ find input:"""
                        , """      ✗ has tag "input\""""
                        , """- <textarea> associated to <label> by id"""
                        , """    ✓ check label exists:"""
                        , """      ✓ has tag "label\""""
                        , """      ✓ has attribute "htmlFor" "field-1\""""
                        , """      ✓ has text "Field 1\""""
                        , """    ✗ find textarea:"""
                        , """      ✗ has tag "textarea\""""
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
                        , "            <b></b>"
                        , "        </label>"
                        , "    </body>"
                        , ""
                        , ""
                        , """▼ ProgramTest.fillIn "Field 1\""""
                        , ""
                        , "Expected one of the following to exist and have an \"oninput\" handler:"
                        , """- <input> with parent <label>"""
                        , """    ✓ find label:"""
                        , """      ✓ has tag "label\""""
                        , """      ✓ has containing [ text "Field 1" ]"""
                        , """    ✗ has tag "input\""""
                        , """- <textarea> with parent <label>"""
                        , """    ✓ find label:"""
                        , """      ✓ has tag "label\""""
                        , """      ✓ has containing [ text "Field 1" ]"""
                        , """    ✗ has tag "textarea\""""
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
                        , """- <input> associated to <label> by id"""
                        , """    ✗ check label exists:"""
                        , """      ✗ has tag "label\""""
                        , """- <input> with aria-label and id"""
                        , """    ✗ has tag "input\""""
                        , """- <textarea> associated to <label> by id"""
                        , """    ✗ check label exists:"""
                        , """      ✗ has tag "label\""""
                        , """- <textarea> with aria-label and id"""
                        , """    ✗ has tag "textarea\""""
                        ]
        ]
