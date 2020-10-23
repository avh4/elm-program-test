module TestResult exposing (TestResult, andThen, fail, toHistory)

import ProgramTest.Failure exposing (Failure)
import TestState exposing (TestState)


type alias TestResult model msg effect =
    Result
        { history : List model
        , reason : Failure
        }
        (TestState model msg effect)


fail : Failure -> TestState model msg effect -> TestResult model msg effect
fail failure state =
    Err
        { history = TestState.toHistory state
        , reason = failure
        }


toHistory : TestResult model msg effect -> List model
toHistory result =
    case result of
        Ok state ->
            TestState.toHistory state

        Err { history } ->
            history


andThen : (TestState model msg effect -> Result Failure (TestState model msg effect)) -> TestResult model msg effect -> TestResult model msg effect
andThen f testResult =
    case testResult of
        Ok state ->
            case f state of
                Err failure ->
                    fail failure state

                Ok newState ->
                    Ok newState

        Err _ ->
            testResult
