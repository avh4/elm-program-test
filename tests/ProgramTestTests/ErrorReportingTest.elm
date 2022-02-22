module ProgramTestTests.ErrorReportingTest exposing (all)

import Html
import Html.Attributes as HA
import ProgramTest
import Test exposing (..)
import Test.Expect exposing (expectFailureModifiedBy)
import TestingProgram


all : Test
all =
    describe "error reporting"
        [ test "reduces HTML to only what's relevant" <|
            \() ->
                TestingProgram.startView
                    (Html.div [ HA.class "container" ]
                        [ Html.div [ HA.class "header" ]
                            [ Html.ul [ HA.class "nav" ]
                                [ Html.li [ HA.class "nav-item" ] [ Html.text "A" ]
                                , Html.li [ HA.class "nav-item" ] [ Html.text "B" ]
                                , Html.li [ HA.class "nav-item" ] [ Html.text "C" ]
                                , Html.li [ HA.class "nav-item" ] [ Html.text "D" ]
                                ]
                            ]
                        , Html.form []
                            [ Html.input [ HA.name "name" ] []
                            , Html.button [] [ Html.text "Save" ]
                            ]
                        , Html.div [ HA.id "footer" ]
                            [ Html.div [ HA.class "col-3-1" ] [ Html.text "" ]
                            , Html.div [ HA.class "col-3-1" ] []
                            , Html.div [ HA.class "col-3-1" ] []
                            ]
                        ]
                    )
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.done
                    |> expectFailureModifiedBy (removeRightFrom "\n\n\n▼ ProgramTest.clickButton")
                        [ "▼ Query.fromHtml"
                        , ""
                        , "    <body>"
                        , "        <div class=\"container\">"
                        , "            <div class=\"header\">...</div>"
                        , "            <form>"
                        , "                <input name=\"name\"></input>"
                        , "                <button>"
                        , "                    Save"
                        , "                </button>"
                        , "            </form>"
                        , "            <div id=\"footer\">...</div>"
                        , "        </div>"
                        , "    </body>"
                        ]
        ]


removeRightFrom : String -> String -> String
removeRightFrom match input =
    case String.indices match input of
        first :: _ ->
            String.left first input

        _ ->
            input
