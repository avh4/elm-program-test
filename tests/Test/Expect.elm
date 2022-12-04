module Test.Expect exposing (expectAnyFailure, expectFailure, expectFailureContaining, expectFailureModifiedBy, expectSuccess)

{-| Functions for asserting things about expectations.
-}

import Expect exposing (Expectation)
import Test.Runner


expectSuccess : Expectation -> Expectation
expectSuccess actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.pass

        Just actualInfo ->
            Expect.fail ("expectSuccess: Expected a success, but got a failure:\n" ++ actualInfo.description)


expectFailure : List String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    expectFailureModifiedBy identity expectedFailureMessage actualResult


expectFailureModifiedBy : (String -> String) -> List String -> Expectation -> Expectation
expectFailureModifiedBy modify expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            actualInfo.description
                |> String.replace "\u{001B}[0m" ""
                |> String.replace "\u{001B}[1m" ""
                |> String.replace "\u{001B}[2m" ""
                |> String.replace "\u{001B}[22m" ""
                |> String.replace "\u{001B}[31m" ""
                |> String.replace "\u{001B}[32m" ""
                |> String.replace "\u{001B}[37m" ""
                |> String.replace "\u{001B}[39m" ""
                |> modify
                |> Expect.equal (String.join "\n" expectedFailureMessage)


expectAnyFailure : Expectation -> Expectation
expectAnyFailure actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            Expect.pass


expectFailureContaining : String -> Expectation -> Expectation
expectFailureContaining expectedSubstring actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            String.contains expectedSubstring actualInfo.description
                |> Expect.equal True
                |> Expect.onFail
                    ("Expected failure message to contain: "
                        ++ expectedSubstring
                        ++ "but got\n\n"
                        ++ actualInfo.description
                    )
