module ProgramTest.TestHtmlHacksTest exposing (all)

import Expect
import Html.Parser exposing (Node(..))
import ProgramTest.TestHtmlHacks as TestHtmlHacks
import ProgramTest.TestHtmlParser as TestHtmlParser exposing (Assertion(..), FailureReport(..), Selector(..), Step(..))
import Test exposing (..)


all : Test
all =
    describe "ProgramTest.TestHtmlHacks"
        [ describe "parseFailureReason"
            [ test "parses a selector error" <|
                \() ->
                    TestHtmlHacks.parseFailureReport
                        "▼ Query.fromHtml\n\n    <body>\n        <button disabled=true>\n            Click Me\n        </button>\n    </body>\n\n\n▼ Query.has [ tag \"form\", containing [ tag \"input\", attribute \"type\" \"submit\", attribute \"value\" \"Click Me\" ]  ]\n\n✓ has tag \"form\"\n✗ has containing [ tag \"input\", attribute \"type\" \"submit\", attribute \"value\" \"Click Me\" ] "
                        |> Result.andThen getAssertion
                        |> Expect.equal
                            (Ok
                                (Has
                                    [ Tag "form"
                                    , Containing
                                        [ Tag "input"
                                        , Attribute "type" "submit"
                                        , Attribute "value" "Click Me"
                                        ]
                                    ]
                                    [ Ok "has tag \"form\""
                                    , Err "has containing [ tag \"input\", attribute \"type\" \"submit\", attribute \"value\" \"Click Me\" ]"
                                    ]
                                )
                            )
            ]
        , describe "parseSimulateFailure"
            [ test "parses" <|
                \() ->
                    TestHtmlHacks.parseSimulateFailure "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would.\n\n<button>\n    Click Me\n</button>"
                        |> Expect.equal "Event.expectEvent: I found a node, but it does not listen for \"click\" events like I expected it would."
            ]
        , describe "parseFailureReport" <|
            let
                parse =
                    TestHtmlHacks.parseFailureReport << String.join "\n"
            in
            [ test "parses initial HTML" <|
                \() ->
                    parse
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <div>"
                        , "            <form id=\"formA\">"
                        , "                <button>"
                        , "                </button>"
                        , "            </form>"
                        , "        </div>"
                        , "        Outer text"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ Query.has [ tag \"form\", text \"ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%\" ]"
                        , ""
                        , "✓ has tag \"form\""
                        , "✗ has text \"ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%\""
                        ]
                        |> Result.andThen getHtml
                        |> Expect.equal
                            (Ok
                                (Element "body"
                                    []
                                    [ Element "div"
                                        []
                                        [ Element "form"
                                            [ ( "id", "formA" ) ]
                                            [ Element "button" [] []
                                            ]
                                        ]
                                    , Html.Parser.Text "Outer text"
                                    ]
                                )
                            )
            , test "parses assertion with selector details" <|
                \() ->
                    parse
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <div>"
                        , "            <form id=\"formA\">"
                        , "                <button>"
                        , "                </button>"
                        , "            </form>"
                        , "        </div>"
                        , "        Outer text"
                        , "    </body>"
                        , ""
                        , ""
                        , "▼ Query.has [ tag \"form\", text \"ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%\" ]"
                        , ""
                        , "✓ has tag \"form\""
                        , "✗ has text \"ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%\""
                        ]
                        |> Result.andThen getAssertion
                        |> Expect.equal
                            (Ok
                                (Has
                                    [ Tag "form"
                                    , TestHtmlParser.Text "ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%"
                                    ]
                                    [ Ok "has tag \"form\""
                                    , Err "has text \"ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%\""
                                    ]
                                )
                            )
            , test "parses Query.find steps" <|
                \() ->
                    parse
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
                        , "▼ Query.find [ tag \"label\", containing [ text \"Field 1\" ]  ]"
                        , ""
                        , "    1)  <label>"
                        , "            Field 1"
                        , "            <b>"
                        , "            </b>"
                        , "        </label>"
                        , ""
                        , ""
                        , "▼ Query.has [ tag \"textarea\" ]"
                        , ""
                        , "✗ has tag \"textarea\""
                        ]
                        |> Result.andThen getSteps
                        |> Expect.equal
                            (Ok
                                [ FindStep
                                    [ Tag "label"
                                    , Containing [ TestHtmlParser.Text "Field 1" ]
                                    ]
                                    (Element "label"
                                        []
                                        [ Html.Parser.Text "Field 1"
                                        , Element "b" [] []
                                        ]
                                    )
                                ]
                            )
            , test "parses an expectEvent failure" <|
                \() ->
                    parse
                        [ "Event.expectEvent: I found a node, but it does not listen for \"change\" events like I expected it would."
                        , ""
                        , "<select id=\"name-select\">"
                        , "    <option value=\"hamster-value\">"
                        , "        Hamster"
                        , "    </option>"
                        , "    <option value=\"george-value\">"
                        , "        George"
                        , "    </option>"
                        , "</select>"
                        ]
                        |> Expect.equal
                            (Ok
                                (EventFailure "change"
                                    (Element "select"
                                        [ ( "id", "name-select" ) ]
                                        [ Element "option"
                                            [ ( "value", "hamster-value" ) ]
                                            [ Html.Parser.Text "Hamster"
                                            ]
                                        , Element "option"
                                            [ ( "value", "george-value" ) ]
                                            [ Html.Parser.Text "George"
                                            ]
                                        ]
                                    )
                                )
                            )
            ]
        ]


getHtml : FailureReport Html.Parser.Node -> Result String Html.Parser.Node
getHtml report =
    case report of
        QueryFailure node _ _ ->
            Ok node

        _ ->
            Err ("Expected QueryFailure, but got: " ++ Debug.toString report)


getAssertion : FailureReport html -> Result String Assertion
getAssertion report =
    case report of
        QueryFailure _ _ assertion ->
            Ok assertion

        _ ->
            Err ("Expected QueryFailure, but got: " ++ Debug.toString report)


getSteps : FailureReport html -> Result String (List (Step html))
getSteps report =
    case report of
        QueryFailure _ steps _ ->
            Ok steps

        _ ->
            Err ("Expected QueryFailure, but got: " ++ Debug.toString report)
