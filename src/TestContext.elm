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
        , fillInTextarea
        , routeChange
        , shouldHave
        , shouldHaveLastEffect
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
@docs fillInTextarea
@docs routeChange


## Directly sending Msgs

@docs update


## Final assertions

@docs expectViewHas, expectView
@docs expectLastEffect, expectModel


## Intermediate assertions

These functions can be used to make assertions on a `TestContext` without ending the test.

@docs shouldHave, shouldNotHave, shouldHaveView
@docs shouldHaveLastEffect

To end a `TestContext` without using a [final assertion](#final-assertions), use the following function:

@docs done


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


{-| A `TestContext` represents an Elm program, a current state for that program,
and a log of any errors that have occurred while simulating interaction with the program.

  - To create a `TestContext, see the`create*` functions below.
  - To advance the state of a `TestContext`, see [Simulating user input](#simulating-user-input), and [Directly sending Msgs](#directly-sending-msgs)
  - To assert on the resulting state of a `TestContext`, see [Final assertions](#final-assertions)

-}
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


{-| Creates a `TestContext` from the parts of a standard `Html.program`.

See other `create*` functions below if the program you want to test
uses flags or nagivation.

-}
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


{-| Creates a `TestContext` from the parts of a standard with flags (`Html.programWithFlags`).

If your program uses `Json.Encode.Value` as its flags type,
you may find [`createWithJsonStringFlags`](#createWithJsonStringFlags) useful.

If your program does not use flags, see [`create`](#create).

See other `create*` functions below if the program you want to test
uses nagivation.

-}
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


{-| A simplified way to create a `TestContext` for a program that decodes its flags with a JSON decoder.

If your program does not use `Json.Encode.Value` as its flags type,
or you want more control over how the flags are provided in your tests,
see [`createWithFlags`](#createWithFlags).

If your program does not use flags, see [`create`](#create).

See other `create*` functions below if the program you want to test
uses nagivation.

-}
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


{-| Creates a `TestContext` from the parts of a `Navigation.program`.

See other `create*` functions above and below if the program you want to test
uses flags or does not use nagivation.

-}
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


{-| Creates a `TestContext` from the parts of a program with navigation and flags (`Navigation.programWithFlags`).

If your program uses `Json.Encode.Value` as its flags type,
you may find [`createWithNavigationAndJsonStringFlags`](#createWithNavigationAndJsonStringFlags) useful.

If your program does not use flags, see [`createWithNavigation`](#createWithNavigation).

If your program does not use navigation, see [`createWithFlags`](#createWithFlags).

-}
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


{-| A simplified way to create a `TestContext` for a program with navigation that decodes its flags with a JSON decoder.

If your program does not use `Json.Encode.Value` as its flags type,
or you want more control over how the flags are provided in your tests,
see [`createWithNavigationAndFlags`](#createWithNavigationAndFlags).

If your program does not use flags, see [`createWithNavigation`](#createWithNavigation).

If your program does not use navigation, see [`createWithJsonStringFlags`](#createWithJsonStringFlags).

-}
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


{-| Advances the state of the `TestContext`'s program by using the `TestContext`'s program's update function
with the given `msg`.

This can be used to simulate events that can only be triggered by [commands (`Cmd`) and subscriptions (`Sub`)](https://guide.elm-lang.org/architecture/effects/)
(i.e., that cannot be triggered by user interaction with the view).

NOTE: When possible, you should prefer [Simulating user input](#simulating-user-input),
as doing so will make your tests more robust to changes in your program's implementation details.

-}
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


{-| Simulates a custom DOM event.

NOTE: If there is another, more specific function (see [Simulating user input](#simulating-user-input)
that does what you want, prefer that instead, as you will get the benefit of better error messages.

Parameters:

  - `findTarget`: A function to find the HTML element that responds to the event
    (typically this will be a call to `Test.Html.Query.find [ ...some selector... ]`)
  - `( eventName, eventValue )`: The event to simulate (see [Test.Html.Event "Event Builders"](http://package.elm-lang.org/packages/eeue56/elm-html-test/5.2.0/Test-Html-Event#event-builders))

-}
simulate : (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulate findTarget ( eventName, eventValue ) testContext =
    simulateHelper ("simulate " ++ toString eventName) findTarget ( eventName, eventValue ) testContext


{-| Simulates clicking a button.

Currently, this function will find and click a `<button>` HTML node containing the given `buttonText`.

NOTE: In the future, this function will be generalized to find buttons with accessibility attributes
matching the given `buttonText`.

-}
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


{-| Simulates replace the test in a `<textarea>`.
This function requires that there is only a single `<textarea>` in the view.

If your view has more than one `<textarea>`, use [`simulate`](#simulate).
NOTE: TODO: in the future there will also be `fillIn "<label>"`, and `within (...)`

-}
fillInTextarea : String -> TestContext msg model effect -> TestContext msg model effect
fillInTextarea newContent testContext =
    simulateHelper "fillInTextarea"
        (Query.find [ Selector.tag "textarea" ])
        (Test.Html.Event.input newContent)
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


{-| Make an assertion about the current state of a `TestContext`'s model.
-}
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


expectLastEffectHelper : String -> (effect -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
expectLastEffectHelper functionName assertion (TestContext result) =
    TestContext <|
        case result of
            Err err ->
                Err err

            Ok ( _, ( _, lastEffect ), _ ) ->
                case assertion lastEffect |> Test.Runner.getFailureReason of
                    Nothing ->
                        result

                    Just reason ->
                        Err (ExpectFailed functionName reason.description reason.reason)


{-| Validates the last effect produced by a `TestContext`'s program without ending the `TestContext`.
-}
shouldHaveLastEffect : (effect -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
shouldHaveLastEffect assertion testContext =
    expectLastEffectHelper "shouldHaveLastEffect" assertion testContext


{-| Makes an assertion about the last effect produced by a `TestContext`'s program.
-}
expectLastEffect : (effect -> Expectation) -> TestContext msg model effect -> Expectation
expectLastEffect assertion testContext =
    testContext
        |> expectLastEffectHelper "expectLastEffect" assertion
        |> done


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


{-| Validates the the current state of a `TestContext`'s view without ending the `TestContext`.
-}
shouldHaveView : (Query.Single msg -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
shouldHaveView assertion testContext =
    expectViewHelper "shouldHaveView" assertion testContext


{-| `shouldHave [...selector...]` is equivalent to `shouldHaveView (Test.Html.Query.has [...selector...])`
-}
shouldHave : List Selector.Selector -> TestContext msg model effect -> TestContext msg model effect
shouldHave selector testContext =
    expectViewHelper "shouldHave" (Query.has selector) testContext


{-| `shouldNotHave [...selector...]` is equivalent to `shouldHaveView (Test.Html.Query.hasNot [...selector...])`
-}
shouldNotHave : List Selector.Selector -> TestContext msg model effect -> TestContext msg model effect
shouldNotHave selector testContext =
    expectViewHelper "shouldNotHave" (Query.hasNot selector) testContext


{-| Makes an assertion about the current state of a `TestContext`'s view.
-}
expectView : (Query.Single msg -> Expectation) -> TestContext msg model effect -> Expectation
expectView assertion testContext =
    testContext
        |> expectViewHelper "expectView" assertion
        |> done


{-| A simpler way to assert that a `TestContext`'s view matches a given selector.

`expectViewHas [...selector...]` is the same as `expectView (Test.Html.Query.has [...selector...])`.

-}
expectViewHas : List Selector.Selector -> TestContext msg model effect -> Expectation
expectViewHas selector testContext =
    testContext
        |> expectViewHelper "expectViewHas" (Query.has selector)
        |> done


{-| Ends a `TestContext`, reporting any errors that occurred.

NOTE: You should prefer using a [final assertion](#final-assertions) to end your test over using `done`,
as doing so will [make the intent of your test more clear](https://www.artima.com/weblogs/viewpost.jsp?thread=35578).

-}
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
