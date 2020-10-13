module TestDebugger exposing (main)

import Css exposing (..)
import Html as RootHtml
import Html.Styled as Html exposing (..)
import ProgramTest exposing (toLog)
import VoterRegistrationExampleTest exposing (successfulRegistration)


main : RootHtml.Html ()
main =
    styled div
        [ minWidth (vw 100)
        , minHeight (vh 100)
        , backgroundColor palette.background
        ]
        []
        [ styled div
            [ padding (px 20) ]
            []
            [ styled div
                [ backgroundColor (hex "#FFFFFF")
                , border3 (px 1) solid palette.border
                , boxShadow5 (px 0) (px 0) (px 5) (px 2) palette.shadow
                ]
                []
                [ programView
                ]
            ]
        ]
        |> toUnstyled


programView : Html ()
programView =
    let
        testLog =
            toLog successfulRegistration
    in
    case testLog.history of
        mostRecentModel :: _ ->
            testLog.view mostRecentModel
                |> fromUnstyled
                |> Html.map (\_ -> ())

        _ ->
            Html.text "No model created."


palette =
    { background = hex "#FAFFE2"
    , shadow = hex "#3D737C50"
    , border = hex "#ABCDDD"

    -- Colors from https://www.colourlovers.com/palette/26883/Amazing...
    --Minnie Pearly #FAFFE2
    --Pale Blue Eyes #ABCDDD
    --Teal I hear it... #2A9191
    --Dirty Lagoon #3D737C
    --Mulberry Waltz #825770
    }
