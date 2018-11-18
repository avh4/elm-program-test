module TestContext exposing
    ( TestContext
    , create, createWithFlags, createWithJsonStringFlags, createWithBaseUrl
    , createWithNavigation, createWithNavigationAndFlags, createWithNavigationAndJsonStringFlags
    , clickButton, clickLink
    , fillIn, fillInTextarea
    , check
    , routeChange
    , simulate
    , within
    , update
    , expectViewHas, expectView
    , expectLastEffect, expectModel
    , expectPageChange
    , shouldHave, shouldNotHave, shouldHaveView
    , shouldHaveLastEffect
    , done
    , fail
    )

{-| A `TestContext` simulates the execution of an Elm program.


## Creating

@docs TestContext
@docs create, createWithFlags, createWithJsonStringFlags, createWithBaseUrl
@docs createWithNavigation, createWithNavigationAndFlags, createWithNavigationAndJsonStringFlags


## Simulating user input

@docs clickButton, clickLink
@docs fillIn, fillInTextarea
@docs check
@docs routeChange


## Simulating user input (advanced)

@docs simulate
@docs within


## Directly sending Msgs

@docs update


## Final assertions

@docs expectViewHas, expectView
@docs expectLastEffect, expectModel
@docs expectPageChange


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

import Browser
import Browser.Navigation
import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Json.Encode
import Query.Extra
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Runner
import Test.Runner.Failure
import Url exposing (Url)
import Url.Extra


{-| A `TestContext` represents an Elm program, a current state for that program,
and a log of any errors that have occurred while simulating interaction with the program.

  - To create a `TestContext`, see the `create*` functions below.
  - To advance the state of a `TestContext`, see [Simulating user input](#simulating-user-input), and [Directly sending Msgs](#directly-sending-msgs)
  - To assert on the resulting state of a `TestContext`, see [Final assertions](#final-assertions)

-}
type TestContext msg model effect
    = Active ( TestProgram msg model effect, ( model, effect ), Maybe Url )
    | Finished Failure


type alias TestProgram msg model effect =
    { update : msg -> model -> ( model, effect )
    , view : model -> Query.Single msg
    , onRouteChange : Url -> Maybe msg
    }


type Failure
    = ChangedPage String Url
      -- Errors
    | ExpectFailed String String Test.Runner.Failure.Reason
    | SimulateFailed String String
    | SimulateFailedToFindTarget String String
    | InvalidLocationUrl String String
    | InvalidFlags String String
    | ProgramDoesNotSupportNavigation String
    | NoBaseUrl String String
    | CustomFailure String String


createHelper :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    , onRouteChange : Url -> Maybe msg
    , initialLocation : Maybe Url
    }
    -> TestContext msg model effect
createHelper program =
    Active
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


{-| Creates a `TestContext` from the parts of a standard `Html.program`
and a base URL against which relative URLs can be resolved.

If you don't need to simulate clicking links with relative URLs,
prefer [`create`](#create).

If your program uses navigation, see [`createWithNavigation`](#createWithNavigation).

-}
createWithBaseUrl :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    }
    -> String
    -> TestContext msg model effect
createWithBaseUrl program baseUrl =
    createHelper
        { init = program.init
        , update = program.update
        , view = program.view
        , onRouteChange = \_ -> Nothing
        , initialLocation = Url.Extra.locationFromString baseUrl
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
            Finished (InvalidFlags "createWithJsonStringFlags" (Json.Decode.errorToString message))

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
    (Url -> msg)
    ->
        { init : Url -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> TestContext msg model effect
createWithNavigation onRouteChange program initialUrl =
    case Url.Extra.locationFromString initialUrl of
        Nothing ->
            Finished (InvalidLocationUrl "createWithNavigation" initialUrl)

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
    (Url -> msg)
    ->
        { init : flags -> Url -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> flags
    -> TestContext msg model effect
createWithNavigationAndFlags onRouteChange program initialUrl flags =
    case Url.Extra.locationFromString initialUrl of
        Nothing ->
            Finished (InvalidLocationUrl "createWithNavigationAndFlags" initialUrl)

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
    -> (Url -> msg)
    ->
        { init : flags -> Url -> ( model, effect )
        , update : msg -> model -> ( model, effect )
        , view : model -> Html msg
        }
    -> String
    -> String
    -> TestContext msg model effect
createWithNavigationAndJsonStringFlags flagsDecoder onRouteChange program initialUrl flagsJson =
    case Url.Extra.locationFromString initialUrl of
        Nothing ->
            Finished (InvalidLocationUrl "createWithNavigationAndJsonStringFlags" initialUrl)

        Just location ->
            case Json.Decode.decodeString flagsDecoder flagsJson of
                Err message ->
                    Finished (InvalidFlags "createWithNavigationAndJsonStringFlags" (Json.Decode.errorToString message))

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
update msg testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, ( model, _ ), currentLocation ) ->
            Active
                ( program
                , program.update msg model
                , currentLocation
                )


simulateHelper : String -> (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulateHelper functionDescription findTarget event testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, ( model, _ ), _ ) ->
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
                    Finished (SimulateFailedToFindTarget functionDescription reason.description)

                Nothing ->
                    -- Try to simulate the event, now that we know the target exists
                    case
                        targetQuery
                            |> Test.Html.Event.simulate event
                            |> Test.Html.Event.toResult
                    of
                        Err message ->
                            Finished (SimulateFailed functionDescription message)

                        Ok msg ->
                            update msg testContext


{-| **PRIVATE** helper for simulating events on input elements with associated labels.

NOTE: Currently, this function requires that you also provide the field id
(which must match both the `id` attribute of the target `input` element,
and the `for` attribute of the `label` element).
After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
a future release of this package will remove the `fieldId` parameter.

-}
simulateLabeledInputHelper : String -> String -> String -> Bool -> List Selector -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulateLabeledInputHelper functionDescription fieldId label allowTextArea additionalInputSelectors event testContext =
    testContext
        |> expectViewHelper functionDescription
            (Query.find
                [ Selector.tag "label"
                , Selector.attribute (Html.Attributes.for fieldId)
                , Selector.text label
                ]
                >> Query.has []
            )
        |> simulateHelper functionDescription
            (Query.Extra.oneOf <|
                List.concat
                    [ [ Query.find <|
                            List.concat
                                [ [ Selector.tag "input"
                                  , Selector.id fieldId
                                  ]
                                , additionalInputSelectors
                                ]
                      ]
                    , if allowTextArea then
                        [ Query.find
                            [ Selector.tag "textarea"
                            , Selector.id fieldId
                            ]
                        ]

                      else
                        []
                    ]
            )
            event


{-| Simulates a custom DOM event.

NOTE: If there is another, more specific function (see [Simulating user input](#simulating-user-input)
that does what you want, prefer that instead, as you will get the benefit of better error messages.

Parameters:

  - `findTarget`: A function to find the HTML element that responds to the event
    (typically this will be a call to `Test.Html.Query.find [ ...some selector... ]`)
  - `( eventName, eventValue )`: The event to simulate
    (see [Test.Html.Event "Event Builders"](http://package.elm-lang.org/packages/eeue56/elm-html-test/latest/Test-Html-Event#event-builders))

-}
simulate : (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulate findTarget ( eventName, eventValue ) testContext =
    simulateHelper ("simulate " ++ escapeString eventName) findTarget ( eventName, eventValue ) testContext


escapeString : String -> String
escapeString s =
    "\"" ++ s ++ "\""


{-| Simulates clicking a button.

Currently, this function will find and click a `<button>` HTML node containing the given `buttonText`.

NOTE: In the future, this function will be generalized to find buttons with accessibility attributes
matching the given `buttonText`.

-}
clickButton : String -> TestContext msg model effect -> TestContext msg model effect
clickButton buttonText testContext =
    simulateHelper ("clickButton " ++ escapeString buttonText)
        (Query.find
            [ Selector.tag "button"
            , Selector.containing [ Selector.text buttonText ]
            ]
        )
        Test.Html.Event.click
        testContext


{-| Simulates clicking a `<a href="...">` link.

NOTE: Currently, this function requires that you also provide the expected href.
After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
a future release of this package will remove the `href` parameter.

Note for testing single-page apps,
if the target `<a>` tag has an `onClick` handler,
then the message produced by the handler will be processed
and the `href` will not be followed.
NOTE: Currently this function cannot verify that the onClick handler
sets `preventDefault`, but this will be done in the future after
<https://github.com/eeue56/elm-html-test/issues/63> is resolved.

-}
clickLink : String -> String -> TestContext msg model effect -> TestContext msg model effect
clickLink linkText href testContext =
    let
        functionDescription =
            "clickLink " ++ escapeString linkText

        findLinkTag =
            Query.find
                [ Selector.tag "a"
                , Selector.attribute (Html.Attributes.href href)
                , Selector.containing [ Selector.text linkText ]
                ]

        normalClick =
            ( "click"
            , Json.Encode.object
                [ ( "ctrlKey", Json.Encode.bool False )
                , ( "metaKey", Json.Encode.bool False )
                ]
            )

        ctrlClick =
            ( "click"
            , Json.Encode.object
                [ ( "ctrlKey", Json.Encode.bool True )
                , ( "metaKey", Json.Encode.bool False )
                ]
            )

        metaClick =
            ( "click"
            , Json.Encode.object
                [ ( "ctrlKey", Json.Encode.bool False )
                , ( "metaKey", Json.Encode.bool True )
                ]
            )

        tryClicking { otherwise } tryClickingTestContext =
            case tryClickingTestContext of
                Finished err ->
                    Finished err

                Active ( program, ( model, _ ), _ ) ->
                    let
                        link =
                            program.view model
                                |> findLinkTag
                    in
                    if respondsTo normalClick link then
                        -- there is a click handler
                        -- first make sure the handler properly respects "Open in new tab", etc
                        if respondsTo ctrlClick link || respondsTo metaClick link then
                            tryClickingTestContext
                                |> fail functionDescription
                                    (String.concat
                                        [ "Found an `<a href=\"...\">` tag has an onClick handler, "
                                        , "but the handler is overriding ctrl-click and meta-click.\n\n"
                                        , "A properly behaved single-page app should not override ctrl- and meta-clicks on `<a>` tags "
                                        , "because this prevents users from opening links in new tabs/windows.\n\n"
                                        , "Use `onClickPreventDefaultForLinkWithHref` defined at <https://gist.github.com/avh4/712d43d649b7624fab59285a70610707> instead of `onClick` to fix this problem.\n\n"
                                        , "See discussion of this issue at <https://github.com/elm-lang/navigation/issues/13>."
                                        ]
                                    )

                        else
                            -- everything looks good, so simulate that event and ignore the `href`
                            tryClickingTestContext
                                |> simulateHelper functionDescription findLinkTag normalClick

                    else
                        -- the link doesn't have a click handler
                        tryClickingTestContext |> otherwise

        respondsTo event single =
            case
                single
                    |> Test.Html.Event.simulate event
                    |> Test.Html.Event.toResult
            of
                Err _ ->
                    False

                Ok _ ->
                    True
    in
    testContext
        |> expectViewHelper functionDescription
            (findLinkTag
                >> Query.has []
            )
        |> tryClicking { otherwise = followLink functionDescription href }


followLink : String -> String -> TestContext msg model effect -> TestContext msg model effect
followLink functionDescription href testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( _, _, finalLocation ) ->
            case finalLocation of
                Just location ->
                    Finished (ChangedPage functionDescription (Url.Extra.resolve location href))

                Nothing ->
                    case Url.Extra.locationFromString href of
                        Nothing ->
                            Finished (NoBaseUrl "clickLink" href)

                        Just location ->
                            Finished (ChangedPage functionDescription location)


{-| Simulates replacing the text in an input field labeled with the given label.

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
    simulateLabeledInputHelper ("fillIn " ++ escapeString label)
        fieldId
        label
        True
        [-- TODO: should ensure that known special input types are not set, like `type="checkbox"`, etc?
        ]
        (Test.Html.Event.input newContent)
        testContext


{-| Simulates replacing the text in a `<textarea>`.

This function expects that there is only one `<textarea>` in the view.
If your view has more than one `<textarea>`,
prefer adding associated `<label>` elements and use [`fillIn`](#fillIn).
If you cannot add `<label>` elements see [`within`](#within).

If you need more control over the finding the target element or creating the simulated event,
see [`simulate`](#simulate).

-}
fillInTextarea : String -> TestContext msg model effect -> TestContext msg model effect
fillInTextarea newContent testContext =
    simulateHelper "fillInTextarea"
        (Query.find [ Selector.tag "textarea" ])
        (Test.Html.Event.input newContent)
        testContext


{-| Simulates setting the value of a checkbox labeled with the given label.

NOTE: Currently, this function requires that you also provide the field id
(which must match both the `id` attribute of the target `input` element,
and the `for` attribute of the `label` element).
After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
a future release of this package will remove the `fieldId` parameter.

NOTE: In the future, this will be generalized to work with
aria accessiblity attributes in addition to working with standard HTML label elements.

If you need more control over the finding the target element or creating the simulated event,
see [`simulate`](#simulate).

-}
check : String -> String -> Bool -> TestContext msg model effect -> TestContext msg model effect
check fieldId label willBecomeChecked testContext =
    simulateLabeledInputHelper ("check " ++ escapeString label)
        fieldId
        label
        False
        [ Selector.attribute (Html.Attributes.type_ "checkbox") ]
        (Test.Html.Event.check willBecomeChecked)
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
within findTarget onScopedTest testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, _, _ ) ->
            testContext
                |> replaceView (program.view >> findTarget)
                |> onScopedTest
                |> replaceView program.view


replaceView : (model -> Query.Single msg) -> TestContext msg model effect -> TestContext msg model effect
replaceView newView testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, state, location ) ->
            Active ( { program | view = newView }, state, location )


{-| `url` may be an absolute URL or relative URL
-}
routeChange : String -> TestContext msg model effect -> TestContext msg model effect
routeChange url testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, _, Nothing ) ->
            Finished (ProgramDoesNotSupportNavigation "routeChange")

        Active ( program, _, Just currentLocation ) ->
            case
                Url.Extra.resolve currentLocation url
                    |> program.onRouteChange
            of
                Nothing ->
                    testContext

                Just msg ->
                    update msg testContext


{-| Make an assertion about the current state of a `TestContext`'s model.
-}
expectModel : (model -> Expectation) -> TestContext msg model effect -> Expectation
expectModel assertion testContext =
    done <|
        case testContext of
            Finished err ->
                Finished err

            Active ( _, ( model, _ ), _ ) ->
                case assertion model |> Test.Runner.getFailureReason of
                    Nothing ->
                        testContext

                    Just reason ->
                        Finished (ExpectFailed "expectModel" reason.description reason.reason)


expectLastEffectHelper : String -> (effect -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
expectLastEffectHelper functionName assertion testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( _, ( _, lastEffect ), _ ) ->
            case assertion lastEffect |> Test.Runner.getFailureReason of
                Nothing ->
                    testContext

                Just reason ->
                    Finished (ExpectFailed functionName reason.description reason.reason)


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
expectViewHelper functionName assertion testContext =
    case testContext of
        Finished err ->
            Finished err

        Active ( program, ( model, _ ), _ ) ->
            case
                model
                    |> program.view
                    |> assertion
                    |> Test.Runner.getFailureReason
            of
                Nothing ->
                    testContext

                Just reason ->
                    Finished (ExpectFailed functionName reason.description reason.reason)


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
done testContext =
    case testContext of
        Active _ ->
            Expect.pass

        Finished (ChangedPage cause finalLocation) ->
            Expect.fail (cause ++ " caused the program to end by navigating to " ++ escapeString (Url.toString finalLocation) ++ ".  NOTE: If this is what you intended, use `expectPageChange` instead of `done`.")

        Finished (ExpectFailed expectationName description reason) ->
            Expect.fail (expectationName ++ ":\n" ++ Test.Runner.Failure.format description reason)

        Finished (SimulateFailed functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Finished (SimulateFailedToFindTarget functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Finished (InvalidLocationUrl functionName invalidUrl) ->
            Expect.fail (functionName ++ ": " ++ "Not a valid absolute URL:\n" ++ escapeString invalidUrl)

        Finished (InvalidFlags functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Finished (ProgramDoesNotSupportNavigation functionName) ->
            Expect.fail (functionName ++ ": Program does not support navigation.  Use TestContext.createWithNavigation or related function to create a TestContext that supports navigation.")

        Finished (NoBaseUrl functionName relativeUrl) ->
            Expect.fail (functionName ++ ": The TestContext does not have a base URL and cannot resolve the relative URL " ++ escapeString relativeUrl ++ ".  Use TestContext.createWithBaseUrl to create a TestContext that can resolve relative URLs.")

        Finished (CustomFailure assertionName message) ->
            Expect.fail (assertionName ++ ": " ++ message)


{-| Asserts that the program ended by navigating away to another URL.
-}
expectPageChange : String -> TestContext msg model effect -> Expectation
expectPageChange expectedUrl testContext =
    case testContext of
        Finished (ChangedPage cause finalLocation) ->
            Url.toString finalLocation |> Expect.equal expectedUrl

        Finished _ ->
            testContext |> done

        Active _ ->
            Expect.fail "expectPageChange: expected to have navigated to a different URL, but no links were clicked"


{-| `fail` can be used to report custom errors if you are writing your own convenience functions to deal with test contexts.

Example (this is a function that checks for a particular structure in the program's view,
but will also fail the TestContext if the `expectedCount` parameter is invalid):

    expectNotificationCount : Int -> TestContext Msg Model effect -> TestContext Msg Model effect
    expectNotificationCount expectedCount testContext =
        if expectedCount <= 0 then
            testContext
                |> TestContext.fail "expectNotificationCount"
                    ("expectedCount must be positive, but was: " ++ String.fromInt expectedCount)

        else
            testContext
                |> shouldHave
                    [ Test.Html.Selector.class "notifications"
                    , Test.Html.Selector.text (toString expectedCount)
                    ]

-}
fail : String -> String -> TestContext msg model effect -> TestContext msg model effect
fail assertionName failureMessage testContext =
    case testContext of
        Finished err ->
            Finished err

        Active _ ->
            Finished (CustomFailure assertionName failureMessage)
