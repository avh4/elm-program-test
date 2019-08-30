module TestHelper exposing (testAssertion1, testAssertion2, testAssertion3)

import Expect exposing (Expectation)
import Html
import Html.Events exposing (onClick)
import Json.Decode
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Task as Task
import Test exposing (..)
import Test.Expect exposing (expectFailure, expectSuccess)
import Test.Http



-- Test assertion helpers
--
-- These helpers make it easier to write tests that check both the expect* and ensure*
-- versions of an assertion.


testAssertion1 :
    (a -> ProgramTest model msg effect -> Expectation)
    -> (a -> ProgramTest model msg effect -> ProgramTest model msg effect)
    -> String
    -> (String -> (a -> ProgramTest model msg effect -> Expectation) -> Expectation)
    -> Test
testAssertion1 expect ensure name go =
    describe name
        [ test "expect" <|
            \() ->
                go "expect" expect
        , test "ensure" <|
            \() ->
                go "ensure" (\a -> ensure a >> ProgramTest.done)
        ]


testAssertion2 :
    (a -> b -> ProgramTest model msg effect -> Expectation)
    -> (a -> b -> ProgramTest model msg effect -> ProgramTest model msg effect)
    -> String
    -> (String -> (a -> b -> ProgramTest model msg effect -> Expectation) -> Expectation)
    -> Test
testAssertion2 expect ensure name go =
    describe name
        [ test "expect" <|
            \() ->
                go "expect" expect
        , test "ensure" <|
            \() ->
                go "ensure" (\a b -> ensure a b >> ProgramTest.done)
        ]


testAssertion3 :
    (a -> b -> c -> ProgramTest model msg effect -> Expectation)
    -> (a -> b -> c -> ProgramTest model msg effect -> ProgramTest model msg effect)
    -> String
    -> (String -> (a -> b -> c -> ProgramTest model msg effect -> Expectation) -> Expectation)
    -> Test
testAssertion3 expect ensure name go =
    describe name
        [ test "expect" <|
            \() ->
                go "expect" expect
        , test "ensure" <|
            \() ->
                go "ensure" (\a b c -> ensure a b c >> ProgramTest.done)
        ]
