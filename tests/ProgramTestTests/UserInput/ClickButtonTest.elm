module ProgramTestTests.UserInput.ClickButtonTest exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onSubmit)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Expect exposing (expectAnyFailure, expectFailure)
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
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "CLICK" ])
        , test "can click an elm-ui button" <|
            \() ->
                TestingProgram.startView
                    (Html.div
                        [ onClick (Log "CLICK")
                        , Html.Attributes.attribute "role" "button"
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "CLICK" ])
        , test "can click a submit button (no type attribute) in a form to submit the form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT")
                        ]
                        [ Html.button [] [ Html.text "Click Me" ]
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "SUBMIT" ])
        , test "can click a submit button (type=submit) in a form to submit the form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT")
                        ]
                        [ Html.button [ type_ "submit" ] [ Html.text "Click Me" ]
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "SUBMIT" ])
        , test "can click a submit button (input type=submit) in a form to submit the form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT")
                        ]
                        [ Html.input [ type_ "submit", value "Click Me" ] []
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "SUBMIT" ])
        , test "clicking a <button type=button> does not submit the form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT")
                        ]
                        [ Html.button [ type_ "button" ] [ Html.text "Click Me" ]
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectAnyFailure
        , test "fails when clicking a disabled button" <|
            \() ->
                TestingProgram.startView
                    (Html.button
                        [ onClick (Log "CLICK")
                        , Html.Attributes.disabled True
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "clickButton \"Click Me\": "
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <button disabled=true>"
                        , "            Click Me"
                        , "        </button>"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ Query.has [ text \"HTML expected by the call to: clickButton \"Click Me\"\" ]"
                        , ""
                        , "✗ has text \"HTML expected by the call to: clickButton \"Click Me\"\""
                        , ""
                        , "Expected one of the following to exist:"
                        , "- <button> (not disabled) with text \"Click Me\""
                        , "- an element with role=\"button\" (not disabled) and text \"Click Me\""
                        , "- a <form> with onSubmit containing a <button> (not disabled, not type=button) with text \"Click Me\""
                        , "- a <form> with onSubmit containing an <input type=submit value=\"Click Me\"> (not disabled)"
                        ]
        ]
