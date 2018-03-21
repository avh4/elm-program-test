module TestContext
    exposing
        ( TestContext
        , create
        , done
        , expectModel
        )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext, create


## Assertions

@docs expectModel

-}

import Expect exposing (Expectation)
import Test.Runner
import Test.Runner.Failure


type TestContext model
    = TestContext (Result Failure ( TestProgram, model ))


type alias TestProgram =
    {}


type Failure
    = TODO_NotImplemented
    | ExpectFailed String String Test.Runner.Failure.Reason


create : { init : model } -> TestContext model
create program =
    TestContext (Ok ( {}, program.init ))


expectModel : (model -> Expectation) -> TestContext model -> TestContext model
expectModel assertion (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( program, model ) ->
                case assertion model |> Test.Runner.getFailureReason of
                    Nothing ->
                        Ok ( program, model )

                    Just reason ->
                        Err (ExpectFailed "expectModel" reason.description reason.reason)


done : TestContext model -> Expectation
done (TestContext result) =
    case result of
        Ok _ ->
            Expect.pass

        Err (ExpectFailed expectationName description reason) ->
            Expect.fail (expectationName ++ ": " ++ Test.Runner.Failure.format description reason)

        Err TODO_NotImplemented ->
            Expect.fail (toString TODO_NotImplemented)
