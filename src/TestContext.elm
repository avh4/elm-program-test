module TestContext
    exposing
        ( TestContext
        , create
        , done
        , expectModel
        , update
        )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext, create


## Directly sending Msgs

@docs update


## Assertions

@docs expectModel

-}

import Expect exposing (Expectation)
import Test.Runner
import Test.Runner.Failure


type TestContext msg model
    = TestContext (Result Failure ( TestProgram msg model (Cmd Never), model ))


type alias TestProgram msg model effect =
    { update : msg -> model -> ( model, effect ) }


type Failure
    = TODO_NotImplemented
    | ExpectFailed String String Test.Runner.Failure.Reason


create : { init : model, update : msg -> model -> ( model, Cmd Never ) } -> TestContext msg model
create program =
    TestContext <|
        Ok
            ( { update = program.update
              }
            , program.init
            )


update : msg -> TestContext msg model -> TestContext msg model
update msg (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( program, model ) ->
                Ok
                    ( program
                    , program.update msg model
                        |> Tuple.first
                    )


expectModel : (model -> Expectation) -> TestContext msg model -> TestContext msg model
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


done : TestContext msg model -> Expectation
done (TestContext result) =
    case result of
        Ok _ ->
            Expect.pass

        Err (ExpectFailed expectationName description reason) ->
            Expect.fail (expectationName ++ ": " ++ Test.Runner.Failure.format description reason)

        Err TODO_NotImplemented ->
            Expect.fail (toString TODO_NotImplemented)
