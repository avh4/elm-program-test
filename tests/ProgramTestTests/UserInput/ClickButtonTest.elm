module ProgramTestTests.UserInput.ClickButtonTest exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Attributes exposing (attribute, disabled, type_, value)
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
        , test "failure message when button doesn't have onClick" <|
            \() ->
                TestingProgram.startView
                    (Html.button []
                        [ Html.text "Click Me" ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <button>"
                        , "            Click Me"
                        , "        </button>"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ ProgramTest.clickButton \"Click Me\""
                        , ""
                        , "Expected one of the following to exist:"
                        , "- <button> with text"
                        , "    ✓ find button:"
                        , "      ✓ has tag \"button\""
                        , "      ✓ has containing [ text \"Click Me\" ]"
                        , "    ✗ simulate click:"
                        , "      ✗ Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
                        ]
        , test "can click a button containing an image with alt text" <|
            \() ->
                TestingProgram.startView
                    (Html.button
                        [ onClick (Log "CLICK") ]
                        [ Html.img
                            [ Html.Attributes.src "googoo.png"
                            , Html.Attributes.alt "Click Me"
                            ]
                            []
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal [ "CLICK" ])
        , test "can click a button with an aria-label attribute" <|
            \() ->
                TestingProgram.startView
                    (Html.button
                        [ onClick (Log "CLICK")
                        , attribute "aria-label" "Close"
                        ]
                        [ Html.text "X" ]
                    )
                    |> ProgramTest.clickButton "Close"
                    |> ProgramTest.expectModel (Expect.equal [ "CLICK" ])
        , test "can click a role=\"button\" with an aria-label attribute" <|
            \() ->
                TestingProgram.startView
                    (Html.div
                        [ onClick (Log "CLICK")
                        , attribute "role" "button"
                        , attribute "aria-label" "Close"
                        ]
                        [ Html.text "X" ]
                    )
                    |> ProgramTest.clickButton "Close"
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
                        , disabled True
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <button disabled=true>"
                        , "            Click Me"
                        , "        </button>"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ ProgramTest.clickButton \"Click Me\""
                        , ""
                        , "Expected one of the following to exist:"
                        , "- <button> with text"
                        , "    ✗ find button:"
                        , "      ✓ has tag \"button\""
                        , "      ✓ has containing [ text \"Click Me\" ]"
                        , "      ✗ has attribute \"disabled\" False"
                        ]
        , test "fails when clicking a disabled button in a form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT") ]
                        [ Html.button [ disabled True ] [ Html.text "Click Me" ]
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectAnyFailure
        , test "fails when clicking a disabled <input type=submit> in a form" <|
            \() ->
                TestingProgram.startView
                    (Html.form
                        [ onSubmit (Log "SUBMIT") ]
                        [ Html.input [ type_ "submit", disabled True, value "Click Me" ] []
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectAnyFailure
        , test "failure message with multiple matches" <|
            \() ->
                TestingProgram.startView
                    (Html.div []
                        [ Html.button
                            [ onClick (Log "CLICK") ]
                            [ Html.text "Click Me"
                            ]
                        , Html.button
                            [ onClick (Log "CLICK2")
                            , attribute "role" "button"
                            , attribute "aria-label" "Click Me"
                            ]
                            []
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectFailure
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <div>"
                        , "            <button>"
                        , "                Click Me"
                        , "            </button>"
                        , "            <button aria-label=\"Click Me\" role=\"button\"></button>"
                        , "        </div>"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ ProgramTest.clickButton \"Click Me\""
                        , ""
                        , "Expected one of the following to exist, but there were multiple successful matches:"
                        , "- <button> with text"
                        , "- <button> with aria-label"
                        , ""
                        , "If that's what you intended, use `ProgramTest.within` to focus in on a portion of"
                        , "the view that contains only one of the matches."
                        ]
        , -- https://github.com/avh4/elm-program-test/issues/149
          test "finds role='button' span when another button is also present" <|
            \() ->
                TestingProgram.startView
                    (Html.span []
                        [ Html.span
                            [ Html.Attributes.attribute "role" "button"
                            , Html.Events.onClick (Log "CLICK")
                            , Html.Attributes.disabled False
                            ]
                            [ Html.map never (Html.text "Clickable element") ]
                        , Html.button [] [ Html.text "Panda" ]
                        ]
                    )
                    |> ProgramTest.clickButton "Clickable element"
                    |> ProgramTest.done
        ]
