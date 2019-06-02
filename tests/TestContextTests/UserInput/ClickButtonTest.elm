module TestContextTests.UserInput.ClickButtonTest exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import Test exposing (..)
import Test.Runner
import TestContext exposing (TestContext)
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
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectModel (Expect.equal [ "CLICK" ])
        , test "can click an elm-ui button" <|
            \() ->
                TestingProgram.startView
                    (Html.div
                        [ onClick (Log "CLICK")
                        , Html.Attributes.attribute "role" "button"
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectModel (Expect.equal [ "CLICK" ])
        , test "fails when clicking a disabled button" <|
            \() ->
                TestingProgram.startView
                    (Html.button
                        [ onClick (Log "CLICK")
                        , Html.Attributes.disabled True
                        ]
                        [ Html.text "Click Me" ]
                    )
                    |> TestContext.clickButton "Click Me"
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


expectFailure : List String -> TestContext msg model effect -> Expectation
expectFailure expectedFailureMessage testContext =
    case Test.Runner.getFailureReason (TestContext.done testContext) of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> Expect.equal (String.join "\n" expectedFailureMessage)
