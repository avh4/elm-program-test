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
        , fillIn
        , fillInTextarea
        , routeChange
        , shouldHave
        , shouldHaveLastEffect
        , shouldHaveView
        , shouldNotHave
        , simulate
        , update
        , within
        )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext
@docs create, createWithFlags, createWithJsonStringFlags
@docs createWithNavigation, createWithNavigationAndFlags, createWithNavigationAndJsonStringFlags


## Simulating user input

@docs clickButton, simulate
@docs fillIn, fillInTextarea
@docs routeChange


## Simulating user input (advanced)

@docs within


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
import Html.Attributes
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
    , view : model -> Query.Single msg
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
              , view = program.view >> Query.fromHtml
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


{-| Simulates replace the text in an input field labeled with the given label.

NOTE: Currently, this function requires that you also provide the field id
(which must match both the `id` attribute of the target `input` element,
and the `for` attribute of the `label` element).
After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
a future release of this package will remove the `fieldId` parameter.

NOTE: TODO: In the future, this will be generalized to work with
labeled textareas as well as labeled input fields.
(related to [eeue56/elm-html-test#49](https://github.com/eeue56/elm-html-test/issues/49>))

NOTE: In the future, this will be generalized to work with
aria accessiblity attributes in addition to working with standard HTML label elements.

If you need to target a `<textarea>` that does not have a label,
see [`fillInTextarea`](#fillInTextArea).

If you need more control over the finding the target element or creating the simulated event,
see [`simulate`](#simulate).

-}
fillIn : String -> String -> String -> TestContext msg model effect -> TestContext msg model effect
fillIn fieldId label newContent testContext =
    let
        functionDescription =
            "fillIn " ++ toString label
    in
    testContext
        |> expectViewHelper functionDescription
            (Query.has
                [ Selector.tag "label"
                , Selector.attribute (Html.Attributes.for fieldId)
                , Selector.text label
                ]
            )
        |> simulateHelper functionDescription
            (Query.find
                [ Selector.tag "input"
                , Selector.id fieldId
                ]
            )
            (Test.Html.Event.input newContent)


{-| Simulates replacing the text in a `<textarea>`.

This function expects that there is only one `<textarea>` in the view.
If your view has more than one `<textarea>`, see [`within`](#within).
NOTE: TODO: in the future there will also be `fillIn "<label>"`.

If you need more control over the finding the target element or creating the simulated event,
see [`simulate`](#simulate).

-}
fillInTextarea : String -> TestContext msg model effect -> TestContext msg model effect
fillInTextarea newContent testContext =
    simulateHelper "fillInTextarea"
        (Query.find [ Selector.tag "textarea" ])
        (Test.Html.Event.input newContent)
        testContext


{-| Focus on a part of the view for a particular operation.

For example, if your view produces the following HTML:

```html
<div>
  <div id="sidebar">
    <button>Submit</button>
  </div>
  <div id="content">
    <button>Submit</button>
  </div>
</div>
```

then the following will allow you to simulate clicking the "Submit" button in the sidebar
(simply using `clickButton "Submit"` would fail because there are two buttons matching that text):

    import Test.Html.Query as Query
    import Test.Html.Selector exposing (id)

    testContext
        |> TestContext.within
            (Query.find [ id "sidebar" ])
            (TestContext.clickButton "Submit")
        |> ...

-}
within : (Query.Single msg -> Query.Single msg) -> (TestContext msg model effect -> TestContext msg model effect) -> (TestContext msg model effect -> TestContext msg model effect)
within findTarget onScopedTest ((TestContext result) as testContext) =
    let
        replaceView newView testContext =
            case testContext of
                TestContext (Err err) ->
                    TestContext (Err err)

                TestContext (Ok ( program, state, location )) ->
                    TestContext (Ok ( { program | view = newView }, state, location ))
    in
    case result of
        Err err ->
            TestContext (Err err)

        Ok ( program, _, _ ) ->
            testContext
                |> replaceView (program.view >> findTarget)
                |> onScopedTest
                |> replaceView program.view


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
