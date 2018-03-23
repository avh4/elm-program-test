module TestContext
    exposing
        ( TestContext
        , clickButton
        , create
        , createWithFlags
        , createWithJsonStringFlags
        , createWithNavigation
        , createWithNavigationAndFlags
        , createWithNavigationAndJsonStringFlags
        , done
        , expectLastEffect
        , expectModel
        , expectView
        , expectViewHas
        , fail
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
@docs create, createWithFlags, createWithJsonStringFlags
@docs createWithNavigation, createWithNavigationAndFlags, createWithNavigationAndJsonStringFlags


## Simulating user input

@docs clickButton, simulate
@docs routeChange


## Directly sending Msgs

@docs update


## Final assertions

@docs expectViewHas, expectView, expectLastEffect, expectModel


## Intermediate assertions

These functions can be used to make assertions on a `TestContext` without ending the test.

@docs shouldHave, shouldNotHave, shouldHaveView


## Custom assertions

These functions may be useful if you are writing your own custom assertion functions.

@docs fail

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


type TestContext msg model effect
    = TestContext (Result Failure ( TestProgram msg model effect, ( model, effect ), Maybe Navigation.Location ))


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
    | ProgramDoesNotSupportNavigation String
    | CustomFailure String String


createHelper :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    , onRouteChange : Navigation.Location -> Maybe msg
    , initialLocation : Maybe Navigation.Location
    }
    -> TestContext msg model effect
createHelper program =
    TestContext <|
        Ok
            ( { update = program.update
              , view = program.view
              , onRouteChange = program.onRouteChange
              }
            , program.init
            , program.initialLocation
            )


create :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    }
    -> TestContext msg model effect
create program =
    createHelper
        { init = program.init
        , update = program.update
        , view = program.view
        , onRouteChange = \_ -> Nothing
        , initialLocation = Nothing
        }


createWithFlags :
    { init : flags -> ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    }
    -> flags
    -> TestContext msg model effect
createWithFlags program flags =
    createHelper
        { init = program.init flags
        , update = program.update
        , view = program.view
        , onRouteChange = \_ -> Nothing
        , initialLocation = Nothing
        }


createWithJsonStringFlags :
    Json.Decode.Decoder flags
    ->
        { init : flags -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> TestContext msg model effect
createWithJsonStringFlags flagsDecoder program flagsJson =
    case Json.Decode.decodeString flagsDecoder flagsJson of
        Err message ->
            TestContext <| Err (InvalidFlags "createWithJsonStringFlags" message)

        Ok flags ->
            createHelper
                { init = program.init flags
                , update = program.update
                , view = program.view
                , onRouteChange = \_ -> Nothing
                , initialLocation = Nothing
                }


createWithNavigation :
    (Navigation.Location -> msg)
    ->
        { init : Navigation.Location -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> TestContext msg model effect
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
                , initialLocation = Just location
                }


createWithNavigationAndFlags :
    (Navigation.Location -> msg)
    ->
        { init : flags -> Navigation.Location -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> flags
    -> TestContext msg model effect
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
                , initialLocation = Just location
                }


createWithNavigationAndJsonStringFlags :
    Json.Decode.Decoder flags
    -> (Navigation.Location -> msg)
    ->
        { init : flags -> Navigation.Location -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> String
    -> TestContext msg model effect
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
                        , initialLocation = Just location
                        }


update : msg -> TestContext msg model effect -> TestContext msg model effect
update msg (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( program, ( model, _ ), currentLocation ) ->
                Ok
                    ( program
                    , program.update msg model
                    , currentLocation
                    )


simulateHelper : String -> (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulateHelper functionDescription findTarget event (TestContext result) =
    case result of
        Err err ->
            TestContext <| Err err

        Ok ( program, ( model, _ ), _ ) ->
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


simulate : (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulate findTarget ( eventName, eventValue ) testContext =
    simulateHelper ("simulate " ++ toString eventName) findTarget ( eventName, eventValue ) testContext


clickButton : String -> TestContext msg model effect -> TestContext msg model effect
clickButton buttonText testContext =
    simulateHelper ("clickButton " ++ toString buttonText)
        (Query.find
            [ Selector.tag "button"
            , Selector.containing [ Selector.text buttonText ]
            ]
        )
        Test.Html.Event.click
        testContext


{-| `url` may be an absolute URL or relative URL
-}
routeChange : String -> TestContext msg model effect -> TestContext msg model effect
routeChange url (TestContext result) =
    case result of
        Err err ->
            TestContext <| Err err

        Ok ( program, _, Nothing ) ->
            TestContext <| Err (ProgramDoesNotSupportNavigation "routeChange")

        Ok ( program, _, Just currentLocation ) ->
            case
                Navigation.Extra.resolve currentLocation url
                    |> program.onRouteChange
            of
                Nothing ->
                    TestContext result

                Just msg ->
                    update msg (TestContext result)


expectModel : (model -> Expectation) -> TestContext msg model effect -> Expectation
expectModel assertion (TestContext result) =
    done <|
        TestContext <|
            case result of
                Err err ->
                    Err err

                Ok ( _, ( model, _ ), _ ) ->
                    case assertion model |> Test.Runner.getFailureReason of
                        Nothing ->
                            result

                        Just reason ->
                            Err (ExpectFailed "expectModel" reason.description reason.reason)


expectLastEffect : (effect -> Expectation) -> TestContext msg model effect -> Expectation
expectLastEffect assertion (TestContext result) =
    done <|
        TestContext <|
            case result of
                Err err ->
                    Err err

                Ok ( _, ( _, lastEffect ), _ ) ->
                    case assertion lastEffect |> Test.Runner.getFailureReason of
                        Nothing ->
                            result

                        Just reason ->
                            Err (ExpectFailed "expectLastEffect" reason.description reason.reason)


expectViewHelper : String -> (Query.Single msg -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
expectViewHelper functionName assertion (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( program, ( model, _ ), _ ) ->
                case
                    model
                        |> program.view
                        |> Query.fromHtml
                        |> assertion
                        |> Test.Runner.getFailureReason
                of
                    Nothing ->
                        result

                    Just reason ->
                        Err (ExpectFailed functionName reason.description reason.reason)


shouldHaveView : (Query.Single msg -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
shouldHaveView assertion testContext =
    expectViewHelper "shouldHaveView" assertion testContext


shouldHave : List Selector.Selector -> TestContext msg model effect -> TestContext msg model effect
shouldHave selector testContext =
    expectViewHelper "shouldHave" (Query.has selector) testContext


shouldNotHave : List Selector.Selector -> TestContext msg model effect -> TestContext msg model effect
shouldNotHave selector testContext =
    expectViewHelper "shouldNotHave" (Query.hasNot selector) testContext


expectView : (Query.Single msg -> Expectation) -> TestContext msg model effect -> Expectation
expectView assertion testContext =
    testContext
        |> expectViewHelper "expectView" assertion
        |> done


expectViewHas : List Selector.Selector -> TestContext msg model effect -> Expectation
expectViewHas selector testContext =
    testContext
        |> expectViewHelper "expectViewHas" (Query.has selector)
        |> done


done : TestContext msg model effect -> Expectation
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

        Err (ProgramDoesNotSupportNavigation functionName) ->
            Expect.fail (functionName ++ ": Program does not support navigation.  Use TestContext.createWithNavigation or related function to create a TestContext that supports navigation.")

        Err (CustomFailure assertionName message) ->
            Expect.fail (assertionName ++ ": " ++ message)


{-| `fail` can be used to report custom errors if you are writing your own convenience functions to deal with test contexts.

Example (this is a function that checks for a particular structure in the program's view,
but will also fail the TestContext if the `expectedCount` parameter is invalid):

    expectNotificationCount : Int -> TestContext Msg Model effect -> TestContext Msg Model effect
    expectNotificationCount expectedCount testContext =
        if expectedCount <= 0 then
            testContext
                |> TestContext.fail "expectNotificationCount"
                    ("expectedCount must be positive, but was: " ++ toString expectedCount)
        else
            testContext
                |> shouldHave
                    [ Test.Html.Selector.class "notifications"
                    , Test.Html.Selector.text (toString expectedCount)
                    ]

-}
fail : String -> String -> TestContext msg model effect -> TestContext msg model effect
fail assertionName failureMessage (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok _ ->
                Err (CustomFailure assertionName failureMessage)
