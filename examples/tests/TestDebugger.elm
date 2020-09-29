module TestDebugger exposing (main)

import Html exposing (Html)
import ProgramTest exposing (toLog)
import VoterRegistrationExampleTest exposing (successfulRegistration)


main : Html ()
main =
    let
        testLog =
            toLog successfulRegistration
    in
    case testLog.history of
        mostRecentModel :: _ ->
            Html.map (\_ -> ()) (testLog.view mostRecentModel)

        _ ->
            Html.text "No model created."
