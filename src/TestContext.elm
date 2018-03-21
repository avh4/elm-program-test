module TestContext
    exposing
        ( TestContext
        , clickButton
        , create
        , createWithFlags
        , createWithNavigation
        , createWithNavigationAndFlags
        , createWithNavigationAndJsonStringFlags
        , done
        , expectModel
        , routeChange
        , shouldHave
        , shouldHaveView
        , shouldNotHave
        , simulate
        , update
        )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext
@docs create, createWithFlags
@docs createWithNavigation, createWithNavigationAndFlags, createWithNavigationAndJsonStringFlags


## Simulating user input

@docs clickButton, simulate
@docs routeChange


## Directly sending Msgs

@docs update


## Assertions

@docs shouldHave, shouldNotHave, shouldHaveView
@docs expectModel

-}

import Expect exposing (Expectation)
import Html exposing (Html)
import Json.Decode
import Json.Encode
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
    , onRouteChange : Navigation.Location -> Maybe msg
    }


type Failure
    = ExpectFailed String String Test.Runner.Failure.Reason
    | SimulateFailed String String
    | SimulateFailedToFindTarget String String
    | InvalidLocationUrl String String
    | InvalidFlags String String


createHelper :
    { init : model
    , update : msg -> model -> ( model, Cmd Never )
    , view : model -> Html msg
    , onRouteChange : Navigation.Location -> Maybe msg
    }
    -> TestContext msg model
createHelper program =
    TestContext <|
        Ok
            ( { update = program.update
              , view = program.view
              , onRouteChange = program.onRouteChange
              }
            , program.init
            )


create :
    { init : model
    , update : msg -> model -> ( model, Cmd Never )
    , view : model -> Html msg
    }
    -> TestContext msg model
create program =
    createHelper
        { init = program.init
        , update = program.update
        , view = program.view
        , onRouteChange = \_ -> Nothing
        }


createWithFlags :
    { init : flags -> model
    , update : msg -> model -> ( model, Cmd Never )
    , view : model -> Html msg
    }
    -> flags
    -> TestContext msg model
createWithFlags program flags =
    createHelper
        { init = program.init flags
        , update = program.update
        , view = program.view
        , onRouteChange = \_ -> Nothing
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
            createHelper
                { init = program.init location
                , update = program.update
                , view = program.view
                , onRouteChange = onRouteChange >> Just
                }


createWithNavigationAndFlags :
    (Navigation.Location -> msg)
    ->
        { init : flags -> Navigation.Location -> model
        , update : msg -> model -> ( model, Cmd Never )
        , view : model -> Html msg
        }
    -> String
    -> flags
    -> TestContext msg model
createWithNavigationAndFlags onRouteChange program initialUrl flags =
    case Navigation.Extra.locationFromString initialUrl of
        Nothing ->
            TestContext <| Err (InvalidLocationUrl "createWithNavigationAndFlags" initialUrl)

        Just location ->
            createHelper
                { init = program.init flags location
                , update = program.update
                , view = program.view
                , onRouteChange = onRouteChange >> Just
                }


createWithNavigationAndJsonStringFlags :
    Json.Decode.Decoder flags
    -> (Navigation.Location -> msg)
    ->
        { init : flags -> Navigation.Location -> model
        , update : msg -> model -> ( model, Cmd Never )
        , view : model -> Html msg
        }
    -> String
    -> String
    -> TestContext msg model
createWithNavigationAndJsonStringFlags flagsDecoder onRouteChange program initialUrl flagsJson =
    case Navigation.Extra.locationFromString initialUrl of
        Nothing ->
            TestContext <| Err (InvalidLocationUrl "createWithNavigationAndJsonStringFlags" initialUrl)

        Just location ->
            case Json.Decode.decodeString flagsDecoder flagsJson of
                Err message ->
                    TestContext <| Err (InvalidFlags "createWithNavigationAndJsonStringFlags" message)

                Ok flags ->
                    createHelper
                        { init = program.init flags location
                        , update = program.update
                        , view = program.view
                        , onRouteChange = onRouteChange >> Just
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


simulateHelper : String -> (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model -> TestContext msg model
simulateHelper functionDescription findTarget event (TestContext result) =
    case result of
        Err err ->
            TestContext <| Err err

        Ok ( program, model ) ->
            let
                targetQuery =
                    program.view model
                        |> Query.fromHtml
                        |> findTarget
            in
            -- First check the target so we can give a better error message if it doesn't exist
            case
                targetQuery
                    |> Query.has []
                    |> Test.Runner.getFailureReason
            of
                Just reason ->
                    TestContext <|
                        Err (SimulateFailedToFindTarget functionDescription reason.description)

                Nothing ->
                    -- Try to simulate the event, now that we know the target exists
                    case
                        targetQuery
                            |> Test.Html.Event.simulate event
                            |> Test.Html.Event.toResult
                    of
                        Err message ->
                            TestContext <|
                                Err (SimulateFailed functionDescription message)

                        Ok msg ->
                            update msg (TestContext result)


simulate : (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model -> TestContext msg model
simulate findTarget ( eventName, eventValue ) testContext =
    simulateHelper ("simulate " ++ toString eventName) findTarget ( eventName, eventValue ) testContext


clickButton : String -> TestContext msg model -> TestContext msg model
clickButton buttonText testContext =
    simulateHelper ("clickButton " ++ toString buttonText)
        (Query.find
            [ Selector.tag "button"
            , Selector.containing [ Selector.text buttonText ]
            ]
        )
        Test.Html.Event.click
        testContext


routeChange : String -> TestContext msg model -> TestContext msg model
routeChange url (TestContext result) =
    case result of
        Err err ->
            TestContext <| Err err

        Ok ( program, model ) ->
            case Navigation.Extra.locationFromString url of
                Nothing ->
                    TestContext <| Err (InvalidLocationUrl "routeChange" url)

                Just location ->
                    case program.onRouteChange location of
                        Nothing ->
                            TestContext result

                        Just msg ->
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


expectViewHelper : String -> (Query.Single msg -> Expectation) -> TestContext msg model -> TestContext msg model
expectViewHelper functionName assertion (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( program, model ) ->
                case
                    model
                        |> program.view
                        |> Query.fromHtml
                        |> assertion
                        |> Test.Runner.getFailureReason
                of
                    Nothing ->
                        Ok ( program, model )

                    Just reason ->
                        Err (ExpectFailed functionName reason.description reason.reason)


shouldHaveView : (Query.Single msg -> Expectation) -> TestContext msg model -> TestContext msg model
shouldHaveView assertion testContext =
    expectViewHelper "shouldHaveView" assertion testContext


shouldHave : List Selector.Selector -> TestContext msg model -> TestContext msg model
shouldHave selector testContext =
    expectViewHelper "shouldHave" (Query.has selector) testContext


shouldNotHave : List Selector.Selector -> TestContext msg model -> TestContext msg model
shouldNotHave selector testContext =
    expectViewHelper "shouldNotHave" (Query.hasNot selector) testContext


done : TestContext msg model -> Expectation
done (TestContext result) =
    case result of
        Ok _ ->
            Expect.pass

        Err (ExpectFailed expectationName description reason) ->
            Expect.fail (expectationName ++ ":\n" ++ Test.Runner.Failure.format description reason)

        Err (SimulateFailed functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Err (SimulateFailedToFindTarget functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Err (InvalidLocationUrl functionName invalidUrl) ->
            Expect.fail (functionName ++ ": " ++ "Not a valid absolute URL:\n" ++ toString invalidUrl)

        Err (InvalidFlags functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)
