module ProgramTestTests.UserInput.ClickButtonTest exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Expect exposing (expectFailure)
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
                        [ "clickButton \"Click Me\":"
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <button disabled=true>"
                        , "            Click Me"
                        , "        </button>"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ Query.find [ attribute \"role\" \"button\", containing [ text \"Click Me\" ]  ]"
                        , ""
                        , "0 matches found for this query."
                        , ""
                        , ""
                        , "✗ Query.find always expects to find 1 element, but it found 0 instead."
                        ]
        ]
