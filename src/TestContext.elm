module TestContext
    exposing
        ( TestContext
        , clickButton
        , create
        , createWithFlags
        , createWithNavigation
        , done
        , expectModel
        , update
        )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext, create, createWithFlags, createWithNavigation


## Simulating user input

@docs clickButton


## Directly sending Msgs

@docs update


## Assertions

@docs expectModel

-}

import Expect exposing (Expectation)
import Html exposing (Html)
import Navigation
import Navigation.Extra
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import Test.Runner.Failure


type TestContext msg model
    = TestContext (Result Failure ( TestProgram msg model (Cmd Never), model ))


type alias TestProgram msg model effect =
    { update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    }


type Failure
    = TODO_NotImplemented
    | ExpectFailed String String Test.Runner.Failure.Reason
    | SimulateFailed String String
    | InvalidLocationUrl String String


create :
    { init : model
    , update : msg -> model -> ( model, Cmd Never )
    , view : model -> Html msg
    }
    -> TestContext msg model
create program =
    TestContext <|
        Ok
            ( { update = program.update
              , view = program.view
              }
            , program.init
            )


createWithFlags :
    { init : flags -> model
    , update : msg -> model -> ( model, Cmd Never )
    , view : model -> Html msg
    }
    -> flags
    -> TestContext msg model
createWithFlags program flags =
    create
        { init = program.init flags
        , update = program.update
        , view = program.view
        }


createWithNavigation :
    (Navigation.Location -> msg)
    ->
        { init : Navigation.Location -> model
        , update : msg -> model -> ( model, Cmd Never )
        , view : model -> Html msg
        }
    -> String
    -> TestContext msg model
createWithNavigation onRouteChange program initialUrl =
    case Navigation.Extra.locationFromString initialUrl of
        Nothing ->
            TestContext <| Err (InvalidLocationUrl "createWithNavigation" initialUrl)

        Just location ->
            create
                { init = program.init location
                , update = program.update
                , view = program.view
                }


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


clickButton : String -> TestContext msg model -> TestContext msg model
clickButton buttonText (TestContext result) =
    case result of
        Err err ->
            TestContext <| Err err

        Ok ( program, model ) ->
            case
                program.view model
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.tag "button"
                        , Selector.containing [ Selector.text buttonText ]
                        ]
                    |> Test.Html.Event.simulate Test.Html.Event.click
                    |> Test.Html.Event.toResult
            of
                Err message ->
                    TestContext <|
                        Err (SimulateFailed ("clickButton " ++ toString buttonText) message)

                Ok msg ->
                    update msg (TestContext result)


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

        Err (SimulateFailed functionName message) ->
            Expect.fail (functionName ++ ": " ++ message)

        Err (InvalidLocationUrl functionName invalidUrl) ->
            Expect.fail (functionName ++ ": " ++ "Not a valid absolute URL: " ++ toString invalidUrl)

        Err TODO_NotImplemented ->
            Expect.fail (toString TODO_NotImplemented)
