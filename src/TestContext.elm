module TestContext exposing
    ( TestContext, start
    , createSandbox, createElement, createDocument, createApplication
    , ProgramDefinition
    , withBaseUrl, withJsonStringFlags
    , withSimulatedEffects, SimulatedEffect, SimulatedTask
    , withSimulatedSubscriptions, SimulatedSub
    , clickButton, clickLink
    , fillIn, fillInTextarea
    , check, selectOption
    , routeChange
    , simulate
    , within
    , assertHttpRequestWasMade, assertHttpRequest
    , simulateHttpOk, simulateHttpResponse
    , advanceTime
    , checkAndClearOutgoingPort, simulateIncomingPort
    , update
    , simulateLastEffect
    , expectViewHas, expectView
    , expectLastEffect, expectModel
    , expectPageChange
    , shouldHave, shouldNotHave, shouldHaveView
    , shouldHaveLastEffect
    , done
    , fail, createFailed
    )

{-| A `TestContext` simulates the execution of an Elm program
enabling you write high-level tests for your program.
(High-level tests are valuable in that they provide extremely robust test coverage
in the case of drastic refactorings of your application architecture,
and writing high-level tests helps you focus on the needs an behaviors of your end-users.)

This module allows you to interact with your program by simulating
DOM events (see ["Simulating user input"](#simulating-user-input)) and
external events (see ["Simulating HTTP responses"](#simulating-http-responses),
["Simulating time"](#simulating-time),
and ["Simulating ports"](#simulating-ports)).

After simulating a series of events, you can then check assertions about
the currently rendered state of your program (see ["Final assertions"](#final-assertions)).


# Creating

@docs TestContext, start


## Creating program definitions

A `ProgramDefinition` (required to create a `TextContext` with [`start`](#start))
can be created with one of the following functions that parallel
the functions in `elm/browser`'s `Browser` module.

@docs createSandbox, createElement, createDocument, createApplication
@docs ProgramDefinition


## Options

The following functions allow you to configure your
`ProgramDefinition` before starting it with [`start`](#start).

@docs withBaseUrl, withJsonStringFlags

@docs withSimulatedEffects, SimulatedEffect, SimulatedTask
@docs withSimulatedSubscriptions, SimulatedSub


# Simulating user input

@docs clickButton, clickLink
@docs fillIn, fillInTextarea
@docs check, selectOption
@docs routeChange


## Simulating user input (advanced)

@docs simulate
@docs within


# Simulating external events


## Simulating HTTP responses

@docs assertHttpRequestWasMade, assertHttpRequest
@docs simulateHttpOk, simulateHttpResponse


## Simulating time

@docs advanceTime


## Simulating ports

@docs checkAndClearOutgoingPort, simulateIncomingPort


# Directly sending Msgs

@docs update
@docs simulateLastEffect


# Final assertions

@docs expectViewHas, expectView
@docs expectLastEffect, expectModel
@docs expectPageChange


# Intermediate assertions

These functions can be used to make assertions on a `TestContext` without ending the test.

@docs shouldHave, shouldNotHave, shouldHaveView
@docs shouldHaveLastEffect

To end a `TestContext` without using a [final assertion](#final-assertions), use the following function:

@docs done


# Custom assertions

These functions may be useful if you are writing your own custom assertion functions.

@docs fail, createFailed

-}

import Browser
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Http
import Json.Decode
import Json.Encode
import PairingHeap
import Query.Extra
import SimulatedEffect exposing (SimulatedEffect, SimulatedSub, SimulatedTask)
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Http
import Test.Runner
import Test.Runner.Failure
import TestContext.EffectSimulation as EffectSimulation exposing (EffectSimulation)
import Url exposing (Url)
import Url.Extra


{-| A `TestContext` represents an Elm program, a current state for that program,
and a log of any errors that have occurred while simulating interaction with the program.

  - To create a `TestContext`, see the `create*` functions below.
  - To advance the state of a `TestContext`, see [Simulating user input](#simulating-user-input), and [Directly sending Msgs](#directly-sending-msgs)
  - To assert on the resulting state of a `TestContext`, see [Final assertions](#final-assertions)

-}
type TestContext msg model effect
    = Active
        { program : TestProgram msg model effect (List (SimulatedSub msg))
        , currentModel : model
        , lastEffect : effect
        , currentLocation : Maybe Url
        , effectSimulation : Maybe (EffectSimulation msg effect)
        }
    | Finished Failure


type alias TestProgram msg model effect sub =
    { update : msg -> model -> ( model, effect )
    , view : model -> Query.Single msg
    , onRouteChange : Url -> Maybe msg
    , subscriptions : Maybe (model -> sub)
    }


type Failure
    = ChangedPage String Url
      -- Errors
    | ExpectFailed String String Test.Runner.Failure.Reason
    | SimulateFailed String String
    | SimulateFailedToFindTarget String String
    | SimulateLastEffectFailed String
    | InvalidLocationUrl String String
    | InvalidFlags String String
    | ProgramDoesNotSupportNavigation String
    | NoBaseUrl String String
    | NoMatchingHttpRequest String { method : String, url : String } (List ( String, String ))
    | EffectSimulationNotConfigured String
    | CustomFailure String String


createHelper :
    { init : ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Html msg
    , onRouteChange : Url -> Maybe msg
    }
    -> ProgramOptions msg model effect
    -> TestContext msg model effect
createHelper program options =
    let
        program_ =
            { update = program.update
            , view = program.view >> Query.fromHtml
            , onRouteChange = program.onRouteChange
            , subscriptions = options.subscriptions
            }

        ( newModel, newEffect ) =
            program.init
    in
    Active
        { program = program_
        , currentModel = newModel
        , lastEffect = newEffect
        , currentLocation = options.baseUrl
        , effectSimulation = Maybe.map EffectSimulation.init options.deconstructEffect
        }
        |> queueEffect newEffect
        |> drain


{-| Creates a `ProgramDefinition` from the parts of a `Browser.sandbox` program.

See other `create*` functions below if the program you want to test does not use `Browser.sandbox`.

-}
createSandbox :
    { init : model
    , update : msg -> model -> model
    , view : model -> Html msg
    }
    -> ProgramDefinition () msg model ()
createSandbox program =
    ProgramDefinition emptyOptions <|
        \location () ->
            createHelper
                { init = ( program.init, () )
                , update = \msg model -> ( program.update msg model, () )
                , view = program.view
                , onRouteChange = \_ -> Nothing
                }


{-| Represents an unstarted test program.
Use [`start`](#start) to start the program being tested.
-}
type ProgramDefinition flags msg model effect
    = ProgramDefinition (ProgramOptions msg model effect) (Maybe Url -> flags -> ProgramOptions msg model effect -> TestContext msg model effect)


type alias ProgramOptions msg model effect =
    { baseUrl : Maybe Url
    , deconstructEffect : Maybe (effect -> List (SimulatedEffect msg))
    , subscriptions : Maybe (model -> List (SimulatedSub msg))
    }


emptyOptions : ProgramOptions msg model effect
emptyOptions =
    { baseUrl = Nothing
    , deconstructEffect = Nothing
    , subscriptions = Nothing
    }


{-| Creates a `TestContext` from the parts of a `Browser.element` program.

See other `create*` functions below if the program you want to test does not use `Browser.element`.

If you program has subscriptions that you want to simulate, see [`withSimulatedSubscriptions`](#withSimulatedSubscriptions).

-}
createElement :
    { init : flags -> ( model, effect )
    , view : model -> Html msg
    , update : msg -> model -> ( model, effect )
    }
    -> ProgramDefinition flags msg model effect
createElement program =
    ProgramDefinition emptyOptions <|
        \location flags ->
            createHelper
                { init = program.init flags
                , update = program.update
                , view = program.view
                , onRouteChange = \_ -> Nothing
                }


{-| Starts the given test program by initializing it with the given flags.

If your program uses `Json.Encode.Value` as its flags type,
you may find [`withJsonStringFlags`](#withJsonStringFlags) useful.

-}
start : flags -> ProgramDefinition flags msg model effect -> TestContext msg model effect
start flags (ProgramDefinition options program) =
    program options.baseUrl flags options


{-| Sets the initial browser URL

You must set this when using `createApplication`,
or when using [`clickLink`](#clickLink) and [`expectPageChange`](#expectPageChange)
to simulate a user clicking a link with relative URL.

-}
withBaseUrl : String -> ProgramDefinition flags msg model effect -> ProgramDefinition flags msg model effect
withBaseUrl baseUrl (ProgramDefinition options program) =
    case Url.Extra.locationFromString baseUrl of
        Nothing ->
            ProgramDefinition options (\_ _ _ -> Finished (InvalidLocationUrl "startWithBaseUrl" baseUrl))

        Just url ->
            ProgramDefinition { options | baseUrl = Just url } program


{-| Provides a convenient way to provide flags for a program that decodes flags from JSON.
By providing the JSON decoder, you can then provide the flags as a JSON string when calling
[`start`](#start).
-}
withJsonStringFlags :
    Json.Decode.Decoder flags
    -> ProgramDefinition flags msg model effect
    -> ProgramDefinition String msg model effect
withJsonStringFlags decoder (ProgramDefinition options program) =
    ProgramDefinition options <|
        \location json ->
            case Json.Decode.decodeString decoder json of
                Ok flags ->
                    program location flags

                Err message ->
                    \_ -> Finished (InvalidFlags "withJsonStringFlags" (Json.Decode.errorToString message))


{-| This allows you to provide a function that lets `TestContext` simulate effects that would become `Cmd`s and `Task`s
when your app runs in production
(this enables you to use [`simulateHttpResponse`](#simulateHttpResponse), [`advanceTime`](#advanceTime), etc.).

You only need to use this if you need to [simulate HTTP requests](#simulating-http-responses)
or the [passing of time](#simulating-time).

See the `SimulatedEffect.*` modules in this package for functions that you can use to implement
the required `effect -> List (SimulatedEffect msg)` function for your `effect` type.

-}
withSimulatedEffects :
    (effect -> List (SimulatedEffect msg))
    -> ProgramDefinition flags msg model effect
    -> ProgramDefinition flags msg model effect
withSimulatedEffects fn (ProgramDefinition options program) =
    ProgramDefinition { options | deconstructEffect = Just fn } program


{-| This allows you to provide a function that lets `TestContext` simulate subscriptions that would be `Sub`s
when your app runs in production
(this enables you to use [`simulateIncomingPort`](#simulateIncomingPort), etc.).

You only need to use this if you need to simulate subscriptions in your test.

The function you provide should be similar to your program's `subscriptions` function
but return `SimulatedSub`s instead of `Sub`s.
See the `SimulatedEffect.*` modules in this package for functions that you can use to implement
the required `model -> List (SimulatedSub msg)` function.

-}
withSimulatedSubscriptions :
    (model -> List (SimulatedSub msg))
    -> ProgramDefinition flags msg model effect
    -> ProgramDefinition flags msg model effect
withSimulatedSubscriptions fn (ProgramDefinition options program) =
    ProgramDefinition { options | subscriptions = Just fn } program


{-| Creates a `TestContext` from the parts of a `Browser.document` program.

See other `create*` functions if the program you want to test does not use `Browser.document`.

If you program has subscriptions that you want to simulate, see [`withSimulatedSubscriptions`](#withSimulatedSubscriptions).

-}
createDocument :
    { init : flags -> ( model, effect )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, effect )
    }
    -> ProgramDefinition flags msg model effect
createDocument program =
    ProgramDefinition emptyOptions <|
        \location flags ->
            createHelper
                { init = program.init flags
                , update = program.update
                , view = \model -> Html.node "body" [] (program.view model).body
                , onRouteChange = \_ -> Nothing
                }


{-| Creates a `TestContext` from the parts of a `Browser.application` program.

See other `create*` functions if the program you want to test does not use `Browser.application`.

If you program has subscriptions that you want to simulate, see [`withSimulatedSubscriptions`](#withSimulatedSubscriptions).

-}
createApplication :
    { init : flags -> Url -> () -> ( model, effect )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, effect )
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> ProgramDefinition flags msg model effect
createApplication program =
    ProgramDefinition emptyOptions <|
        \location flags ->
            case location of
                Nothing ->
                    \_ -> Finished (NoBaseUrl "createApplication" "")

                Just url ->
                    createHelper
                        { init = program.init flags url ()
                        , update = program.update
                        , view = \model -> Html.node "body" [] (program.view model).body
                        , onRouteChange = program.onUrlChange >> Just
                        }


{-| This represents an effect that elm-program-test is able to simulate.
When using [`withSimulatedEffects`](#withSimulatedEffects) you will provide a function that can translate
your program's effects into `SimulatedEffect`s.
(If you do not use `withSimulatedEffects`,
then `TestContext` will not simulate any effects for you.)

You can create `SimulatedEffect`s using the the following modules,
which parallel the modules your real program would use to create `Cmd`s and `Task`s:

  - [`SimulatedEffect.Http`](SimulatedEffect-Http) (parallels `Http` from `elm/http`)
  - [`SimulatedEffect.Cmd`](SimulatedEffect-Cmd) (parallels `Platform.Cmd` from `elm/core`)
  - [`SimulatedEffect.Navigation`](SimulatedEffect-Navigation) (parallels `Browser.Navigation` from `elm/browser`)
  - [`SimulatedEffect.Ports`](SimulatedEffect-Ports) (parallels the `port` keyword)
  - [`SimulatedEffect.Task`](SimulatedEffect-Task) (parallels `Task` from `elm/core`)
  - [`SimulatedEffect.Process`](SimulatedEffect-Process) (parallels `Process` from `elm/core`)

-}
type alias SimulatedEffect msg =
    SimulatedEffect.SimulatedEffect msg


{-| Similar to `SimulatedEffect`, but represents a `Task` instead of a `Cmd`.
-}
type alias SimulatedTask x a =
    SimulatedEffect.SimulatedTask x a


{-| This represents a subscription that elm-program-test is able to simulate.
When using [`withSimulatedSubscriptions`](#withSimulatedSubscriptions) you will provide
a function that is similar to your program's `subscriptions` function but that
returns `SimulatedSub`s instead `Sub`s.
(If you do not use `withSimulatedSubscriptions`,
then `TestContext` will not simulate any subscriptions for you.)

You can create `SimulatedSub`s using the the following modules:

  - [`SimulatedEffect.Ports`](SimulatedEffect-Ports) (parallels the `port` keyword)

-}
type alias SimulatedSub msg =
    SimulatedEffect.SimulatedSub msg


{-| Advances the state of the `TestContext`'s by applying the given `msg` to your program's update function
(provided when you created the `TestContext`).

This can be used to simulate events that can only be triggered by [commands (`Cmd`) and subscriptions (`Sub`)](https://guide.elm-lang.org/architecture/effects/)
(i.e., that cannot be triggered by user interaction with the view).

NOTE: When possible, you should prefer [Simulating user input](#simulating-user-input),
[Simulating HTTP responses](#simulating-http-responses),
or (if neither of those support what you need) [`simulateLastEffect`](#simulateLastEffect),
as doing so will make your tests more robust to changes in your program's implementation details.

-}
update : msg -> TestContext msg model effect -> TestContext msg model effect
update msg testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            let
                ( newModel, newEffect ) =
                    state.program.update msg state.currentModel
            in
            Active
                { state
                    | currentModel = newModel
                    , lastEffect = newEffect
                }
                |> queueEffect newEffect
                |> drain


simulateHelper : String -> (Query.Single msg -> Query.Single msg) -> ( String, Json.Encode.Value ) -> TestContext msg model effect -> TestContext msg model effect
simulateHelper functionDescription findTarget event testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            let
                targetQuery =
                    state.program.view state.currentModel
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
        |> (if fieldId == "" then
                identity

            else
                expectViewHelper functionDescription
                    (Query.find
                        [ Selector.tag "label"
                        , Selector.attribute (Html.Attributes.for fieldId)
                        , Selector.text label
                        ]
                        >> Query.has []
                    )
           )
        |> simulateHelper functionDescription
            (Query.Extra.oneOf <|
                List.concat
                    [ if fieldId == "" then
                        [ Query.find
                            [ Selector.tag "label"
                            , Selector.containing [ Selector.text label ]
                            ]
                            >> Query.find [ Selector.tag "input" ]
                        , Query.find
                            [ Selector.tag "input"
                            , Selector.attribute (attribute "aria-label" label)
                            ]
                        ]

                      else
                        [ Query.find <|
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

The parameters are:

1.  The text of the `<a>` tag (which is the link text visible to the user).

2.  The `href` of the `<a>` tag.

    NOTE: After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
    a future release of this package will remove the `href` parameter.

Note for testing single-page apps:
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

                Active state ->
                    let
                        link =
                            state.program.view state.currentModel
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

        Active state ->
            case state.currentLocation of
                Just location ->
                    Finished (ChangedPage functionDescription (Url.Extra.resolve location href))

                Nothing ->
                    case Url.Extra.locationFromString href of
                        Nothing ->
                            Finished (NoBaseUrl "clickLink" href)

                        Just location ->
                            Finished (ChangedPage functionDescription location)


{-| Simulates replacing the text in an input field labeled with the given label.

1.  The id of the input field
    (which must match both the `id` attribute of the target `input` element,
    and the `for` attribute of the `label` element),
    or `""` if the `<input>` is a descendant of the `<label>`.

    NOTE: After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
    a future release of this package will remove this parameter.

2.  The label text of the input field.

3.  The text that will entered into the input field.

There are a few different ways to accessibly label your input fields so that `fillIn` will find them:

  - You can place the `<input>` element inside a `<label>` element that also contains the label text.

    ```html
    <label>
        Favorite fruit
        <input>
    </label>
    ```

  - You can place the `<input>` and a `<label>` element anywhere on the page and link them with a unique id.

    ```html
    <label for="fruit">Favorite fruit</label>
    <input id="fruit"></input>
    ```

  - You can use the `aria-label` attribute.

    ```html
    <input aria-label="Favorite fruit"></input>
    ```

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

The parameters are:

1.  The id of the input field
    (which must match both the `id` attribute of the target `input` element,
    and the `for` attribute of the `label` element),
    or `""` if the `<input>` is a descendant of the `<label>`.

    NOTE: After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
    a future release of this package will remove this parameter.

2.  The label text of the input field

3.  A `Bool` indicating whether to check (`True`) or uncheck (`False`) the checkbox.

NOTE: Currently, this function requires that you also provide the field id
(which must match both the `id` attribute of the target `input` element,
and the `for` attribute of the `label` element).
After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
a future release of this package will remove the `fieldId` parameter.

NOTE: In the future, this will be generalized to work with
aria accessibility attributes in addition to working with standard HTML label elements.

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


{-| Simulates choosing an option with the given text in a select with a given label

The parameters are:

1.  The id of the `<select>`
    (which must match both the `id` attribute of the target `select` element,
    and the `for` attribute of the `label` element),
    or `""` if the `<select>` is a descendant of the `<label>`.

    NOTE: After [eeue56/elm-html-test#52](https://github.com/eeue56/elm-html-test/issues/52) is resolved,
    a future release of this package will remove this parameter.

2.  The label text of the select.

3.  The `value` of the `<option>` that will be chosen.

    NOTE: After [eeue56/elm-html-test#51](https://github.com/eeue56/elm-html-test/issues/51) is resolved,
    a future release of this package will remove this parameter.

4.  The user-visible text of the `<option>` that will be chosen.

Example: If you have a view like the following,

    import Html
    import Html.Attributes exposing (for, id, value)
    import Html.Events exposing (on, targetValue)

    Html.div []
        [ Html.label [ for "pet-select" ] [ Html.text "Choose a pet" ]
        , Html.select
            [ id "pet-select", on "change" targetValue ]
            [ Html.option [ value "dog" ] [ Html.text "Dog" ]
            , Html.option [ value "hamster" ] [ Html.text "Hamster" ]
            ]
        ]

you can simulate selecting an option like this:

    TestContext.selectOption "pet-select" "Choose a pet" "dog" "Dog"

If you need more control over the finding the target element or creating the simulated event,
see [`simulate`](#simulate).

-}
selectOption : String -> String -> String -> String -> TestContext msg model effect -> TestContext msg model effect
selectOption fieldId label optionValue optionText testContext =
    let
        functionDescription =
            String.join " "
                [ "selectOption"
                , escapeString fieldId
                , escapeString label
                , escapeString optionValue
                , escapeString optionText
                ]
    in
    testContext
        |> expectViewHelper functionDescription
            (Query.find
                [ Selector.tag "label"
                , Selector.attribute (Html.Attributes.for fieldId)
                , Selector.text label
                ]
                >> Query.has []
            )
        |> expectViewHelper functionDescription
            (Query.find
                [ Selector.tag "select"
                , Selector.id fieldId
                , Selector.containing
                    [ Selector.tag "option"
                    , Selector.attribute (Html.Attributes.value optionValue)
                    , Selector.text optionText
                    ]
                ]
                >> Query.has []
            )
        |> simulateHelper functionDescription
            (Query.find
                [ Selector.tag "select"
                , Selector.id fieldId
                ]
            )
            ( "change"
            , Json.Encode.object
                [ ( "target"
                  , Json.Encode.object
                        [ ( "value", Json.Encode.string optionValue )
                        ]
                  )
                ]
            )


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

        Active state ->
            testContext
                |> replaceView (state.program.view >> findTarget)
                |> onScopedTest
                |> replaceView state.program.view


withSimulation : (EffectSimulation msg effect -> EffectSimulation msg effect) -> TestContext msg model effect -> TestContext msg model effect
withSimulation f testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            Active
                { state | effectSimulation = Maybe.map f state.effectSimulation }


queueEffect : effect -> TestContext msg model effect -> TestContext msg model effect
queueEffect effect testContext =
    case testContext of
        Finished _ ->
            testContext

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    testContext

                Just simulation ->
                    List.foldl queueSimulatedEffect testContext (simulation.deconstructEffect effect)


queueSimulatedEffect : SimulatedEffect msg -> TestContext msg model effect -> TestContext msg model effect
queueSimulatedEffect effect testContext =
    case testContext of
        Finished _ ->
            testContext

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    testContext

                Just simulation ->
                    case effect of
                        SimulatedEffect.Task t ->
                            Active
                                { state
                                    | effectSimulation =
                                        Just (EffectSimulation.queueTask t simulation)
                                }

                        SimulatedEffect.PortEffect portName value ->
                            Active
                                { state
                                    | effectSimulation =
                                        Just
                                            { simulation
                                                | outgoingPortValues =
                                                    Dict.update portName
                                                        (Maybe.withDefault [] >> (::) value >> Just)
                                                        simulation.outgoingPortValues
                                            }
                                }

                        SimulatedEffect.PushUrl url ->
                            testContext
                                |> routeChange url


drain : TestContext msg model effect -> TestContext msg model effect
drain =
    let
        advanceTimeIfSimulating t testContext =
            case testContext of
                Finished _ ->
                    testContext

                Active state ->
                    case state.effectSimulation of
                        Nothing ->
                            testContext

                        Just _ ->
                            advanceTime t testContext
    in
    advanceTimeIfSimulating 0
        >> drainWorkQueue


drainWorkQueue : TestContext msg model effect -> TestContext msg model effect
drainWorkQueue testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    testContext

                Just simulation ->
                    case EffectSimulation.stepWorkQueue simulation of
                        Nothing ->
                            -- work queue is empty
                            testContext

                        Just ( newSimulation, msg ) ->
                            let
                                updateMaybe m tc =
                                    case m of
                                        Nothing ->
                                            tc

                                        Just m_ ->
                                            update m_ tc
                            in
                            Active { state | effectSimulation = Just newSimulation }
                                |> updateMaybe msg
                                |> drain


{-| A final assertion that checks whether an HTTP request to the specific url and method has been made.

If you want to check the headers or request body, see [`assertHttpRequest`](#assertHttpRequest).

NOTE: You must use [`withSimulatedEffects`](#withSimulatedEffects) before you call [`start`](#start) to be able to use this function.

-}
assertHttpRequestWasMade : String -> String -> TestContext msg model effect -> Expectation
assertHttpRequestWasMade method url testContext =
    done <|
        case testContext of
            Finished err ->
                Finished err

            Active state ->
                case state.effectSimulation of
                    Nothing ->
                        Finished (EffectSimulationNotConfigured "assertHttpRequestWasMade")

                    Just simulation ->
                        if Dict.member ( method, url ) simulation.state.http then
                            testContext

                        else
                            Finished (NoMatchingHttpRequest "assertHttpRequestWasMade" { method = method, url = url } (Dict.keys simulation.state.http))


{-| Allows you to check the details of a pending HTTP request.

See the ["Expectations" section of `Test.Http`](Test-Http#expectations) for functions that might be helpful
in create an expectation on the request.

If you only care about whether the a request was made to the correct URL, see [`assertHttpRequestWasMade`](#assertHttpRequestWasMade).

    ...
        |> assertHttpRequest "POST"
            "https://example.com/save"
            (.body >> Expect.equal """{"content":"updated!"}""")
        |> ...

-}
assertHttpRequest :
    String
    -> String
    -> (SimulatedEffect.HttpRequest msg msg -> Expectation)
    -> TestContext msg model effect
    -> TestContext msg model effect
assertHttpRequest method url checkRequest testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    Finished (EffectSimulationNotConfigured "assertHttpRequest")

                Just simulation ->
                    case Dict.get ( method, url ) simulation.state.http of
                        Just request ->
                            case Test.Runner.getFailureReason (checkRequest request) of
                                Nothing ->
                                    -- check succeeded
                                    testContext

                                Just reason ->
                                    Finished (ExpectFailed "assertHttpRequest" reason.description reason.reason)

                        Nothing ->
                            Finished (NoMatchingHttpRequest "assertHttpRequest" { method = method, url = url } (Dict.keys simulation.state.http))


{-| Simulates an HTTP 200 response to a pending request with the given method and url.

    ...
        |> simulateHttpOk "GET"
            "https://example.com/time.json"
            """{"currentTime":1559013158}"""
        |> ...

If you need to simulate an error, a response with a different status code,
or a response with response headers,
see [`simulateHttpResponse`](#simulateHttpResponse).

If you want to check the request headers or request body, use [`assertHttpRequest`](#assertHttpRequest)
immediately before using `simulateHttpOk`.

NOTE: You must use [`withSimulatedEffects`](#withSimulatedEffects) before you call [`start`](#start) to be able to use this function.

-}
simulateHttpOk : String -> String -> String -> TestContext msg model effect -> TestContext msg model effect
simulateHttpOk method url responseBody =
    simulateHttpResponse method
        url
        (Test.Http.httpResponse
            { statusCode = 200
            , body = responseBody
            , headers = []
            }
        )


{-| Simulates a response to a pending HTTP request.
The test will fail if there is no pending request matching the given method and url.

You may find it helpful to see the ["Responses" section in `Test.Http`](Test-Http#responses)
for convenient ways to create `Http.Response` values.

If you are simulating a 200 OK response and don't need to provide response headers,
you can use the simpler [`simulateHttpOk`](#simulateHttpOk).

If you want to check the request headers or request body, use [`assertHttpRequest`](#assertHttpRequest)
immediately before using `simulateHttpResponse`.

NOTE: You must use [`withSimulatedEffects`](#withSimulatedEffects) before you call [`start`](#start) to be able to use this function.

-}
simulateHttpResponse : String -> String -> Http.Response String -> TestContext msg model effect -> TestContext msg model effect
simulateHttpResponse method url response testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    Finished (EffectSimulationNotConfigured "simulateHttpResponse")

                Just simulation ->
                    case Dict.get ( method, url ) simulation.state.http of
                        Nothing ->
                            Finished (NoMatchingHttpRequest "simulateHttpResponse" { method = method, url = url } (Dict.keys simulation.state.http))

                        Just actualRequest ->
                            withSimulation
                                (EffectSimulation.queueTask (actualRequest.onRequestComplete response))
                                testContext
                                |> drain


{-| Simulates the passing of time.
The `Int` parameter is the number of milliseconds to simulate.
This will cause any pending `Task.sleep`s to trigger if their delay has elapsed.

NOTE: You must use [`withSimulatedEffects`](#withSimulatedEffects) before you call [`start`](#start) to be able to use this function.

-}
advanceTime : Int -> TestContext msg model effect -> TestContext msg model effect
advanceTime delta testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    Finished (EffectSimulationNotConfigured "advanceTime")

                Just simulation ->
                    advanceTo (simulation.state.nowMs + delta) testContext


advanceTo : Int -> TestContext msg model effect -> TestContext msg model effect
advanceTo end testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    Finished (EffectSimulationNotConfigured "advanceTo")

                Just simulation ->
                    let
                        ss =
                            simulation.state
                    in
                    case PairingHeap.findMin simulation.state.futureTasks of
                        Nothing ->
                            -- No future tasks to check
                            Active
                                { state
                                    | effectSimulation =
                                        Just
                                            { simulation
                                                | state = { ss | nowMs = end }
                                            }
                                }

                        Just ( t, task ) ->
                            if t <= end then
                                Active
                                    { state
                                        | effectSimulation =
                                            Just
                                                { simulation
                                                    | state =
                                                        { ss
                                                            | nowMs = t
                                                            , futureTasks = PairingHeap.deleteMin simulation.state.futureTasks
                                                        }
                                                }
                                    }
                                    |> withSimulation (EffectSimulation.queueTask (task ()))
                                    |> drain
                                    |> advanceTo end

                            else
                                -- next task is further in the future than we are advancing
                                Active
                                    { state
                                        | effectSimulation =
                                            Just
                                                { simulation
                                                    | state = { ss | nowMs = end }
                                                }
                                    }


{-| Lets you assert on the values that have been sent to an outgoing port.

The parameters are:

1.  The name of the port
2.  A JSON decoder corresponding to the type of the port
3.  A function that will receive the list of values sent to the port
    since the last use of `checkAndClearOutgoingPort` (or since the start of the test)
    and returns an `Expectation`

For example:

    ...
        |> checkAndClearOutgoingPort
            "saveApiTokenToLocalStorage"
            Json.Decode.string
            (Expect.equal [ "975774a26612", "920facb1bac0" ])
        |> ...

NOTE: You must use [`withSimulatedEffects`](#withSimulatedEffects) before you call [`start`](#start) to be able to use this function.

-}
checkAndClearOutgoingPort : String -> Json.Decode.Decoder a -> (List a -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
checkAndClearOutgoingPort portName decoder checkValues testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.effectSimulation of
                Nothing ->
                    Finished (EffectSimulationNotConfigured "checkAndClearOutgoingPort")

                Just simulation ->
                    case allOk <| List.map (Json.Decode.decodeValue decoder) <| EffectSimulation.outgoingPortValues portName simulation of
                        Err errs ->
                            Finished (CustomFailure "checkAndClearOutgoingPort: failed to decode port values" (List.map Json.Decode.errorToString errs |> String.join "\n"))

                        Ok values ->
                            case Test.Runner.getFailureReason (checkValues values) of
                                Nothing ->
                                    -- the check passed
                                    Active
                                        { state
                                            | effectSimulation =
                                                Just (EffectSimulation.clearOutgoingPortValues portName simulation)
                                        }

                                Just reason ->
                                    Finished
                                        (ExpectFailed ("checkAndClearOutgoingPort: values sent to port \"" ++ portName ++ "\" did not match")
                                            reason.description
                                            reason.reason
                                        )


allOk : List (Result x a) -> Result (List x) (List a)
allOk results =
    let
        step next acc =
            case ( next, acc ) of
                ( Ok n, Ok a ) ->
                    Ok (n :: a)

                ( Ok _, Err x ) ->
                    Err x

                ( Err n, Ok _ ) ->
                    Err [ n ]

                ( Err n, Err x ) ->
                    Err (n :: x)
    in
    List.foldl step (Ok []) results
        |> Result.map List.reverse
        |> Result.mapError List.reverse


{-| Lets you simulate a value being received on an incoming port.

The parameters are:

1.  The name of the port
2.  The JSON representation of the incoming value

NOTE: You must use [`withSimulatedSubscriptions`](#withSimulatedSubscriptions) before you call [`start`](#start) to be able to use this function.

-}
simulateIncomingPort : String -> Json.Encode.Value -> TestContext msg model effect -> TestContext msg model effect
simulateIncomingPort portName value testContext =
    let
        fail_ =
            fail ("simulateIncomingPort \"" ++ portName ++ "\"")
    in
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.program.subscriptions of
                Nothing ->
                    testContext
                        |> fail_ "you MUST use TestContext.withSimulatedSubscriptions to be able to use simulateIncomingPort"

                Just fn ->
                    let
                        subs =
                            fn state.currentModel

                        matches =
                            List.filterMap
                                (\s ->
                                    case s of
                                        SimulatedEffect.PortSub pname decoder ->
                                            if pname == portName then
                                                Json.Decode.decodeValue decoder value
                                                    |> Result.mapError Json.Decode.errorToString
                                                    |> Just

                                            else
                                                Nothing
                                )
                                subs

                        step r tc =
                            case r of
                                Err message ->
                                    tc
                                        |> fail_
                                            ("the value provided does not match the type that the port is expecting:\n"
                                                ++ message
                                            )

                                Ok msg ->
                                    update msg tc
                    in
                    if matches == [] then
                        testContext
                            |> fail_
                                "the program is not currently subscribed to the port"

                    else
                        List.foldl step testContext matches


replaceView : (model -> Query.Single msg) -> TestContext msg model effect -> TestContext msg model effect
replaceView newView testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            let
                program =
                    state.program
            in
            Active
                { state
                    | program = { program | view = newView }
                }


{-| Simulates a route change event (which would happen when your program is
a `Browser.application` and the user changes the URL in the browser's URL bar.

The parameter may be an absolute URL or relative URL.

-}
routeChange : String -> TestContext msg model effect -> TestContext msg model effect
routeChange url testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case state.currentLocation of
                Nothing ->
                    Finished (ProgramDoesNotSupportNavigation "routeChange")

                Just currentLocation ->
                    case
                        Url.Extra.resolve currentLocation url
                            |> state.program.onRouteChange
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

            Active state ->
                case assertion state.currentModel |> Test.Runner.getFailureReason of
                    Nothing ->
                        testContext

                    Just reason ->
                        Finished (ExpectFailed "expectModel" reason.description reason.reason)


{-| Simulate the outcome of the last effect produced by the program being tested
by providing a function that can convert the last effect into `msg`s.

The function you provide will be called with the effect that was returned by the most recent call to `update` or `init` in the `TestContext`.

  - If it returns `Err`, then that will cause the `TestContext` to enter a failure state with the provided message.
  - If it returns `Ok`, then the list of `msg`s will be applied in order via `TestContext.update`.

NOTE: If you are simulating HTTP response, you should prefer the functions described in ["Simulating HTTP responses"](#simulating-http-responses).

-}
simulateLastEffect : (effect -> Result String (List msg)) -> TestContext msg model effect -> TestContext msg model effect
simulateLastEffect toMsgs testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case toMsgs state.lastEffect of
                Ok msgs ->
                    List.foldl update testContext msgs

                Err message ->
                    Finished (SimulateLastEffectFailed message)


expectLastEffectHelper : String -> (effect -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
expectLastEffectHelper functionName assertion testContext =
    case testContext of
        Finished err ->
            Finished err

        Active state ->
            case assertion state.lastEffect |> Test.Runner.getFailureReason of
                Nothing ->
                    testContext

                Just reason ->
                    Finished (ExpectFailed functionName reason.description reason.reason)


{-| Validates the last effect produced by a `TestContext`'s program without ending the `TestContext`.

NOTE: If you are assert about HTTP requests being made,
you should prefer the functions described in ["Simulating HTTP responses"](#simulating-http-responses).

-}
shouldHaveLastEffect : (effect -> Expectation) -> TestContext msg model effect -> TestContext msg model effect
shouldHaveLastEffect assertion testContext =
    expectLastEffectHelper "shouldHaveLastEffect" assertion testContext


{-| Makes an assertion about the last effect produced by a `TestContext`'s program.

NOTE: If you are assert about HTTP requests being made,
you should prefer the functions described in ["Simulating HTTP responses"](#simulating-http-responses).

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

        Active state ->
            case
                state.currentModel
                    |> state.program.view
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

        Finished (SimulateLastEffectFailed message) ->
            Expect.fail ("simulateLastEffect failed: " ++ message)

        Finished (InvalidLocationUrl functionName invalidUrl) ->
            Expect.fail (functionName ++ ": " ++ "Not a valid absolute URL:\n" ++ escapeString invalidUrl)

        Finished (InvalidFlags functionName message) ->
            Expect.fail (functionName ++ ":\n" ++ message)

        Finished (ProgramDoesNotSupportNavigation functionName) ->
            Expect.fail (functionName ++ ": Program does not support navigation.  Use TestContext.application to create a TestContext that supports navigation.")

        Finished (NoBaseUrl functionName relativeUrl) ->
            Expect.fail (functionName ++ ": The TestContext does not have a base URL and cannot resolve the relative URL " ++ escapeString relativeUrl ++ ".  Use TestContext.startWithBaseUrl to create a TestContext that can resolve relative URLs.")

        Finished (NoMatchingHttpRequest functionName request pendingRequests) ->
            Expect.fail <|
                String.concat
                    [ functionName
                    , ": "
                    , "Expected HTTP request ("
                    , request.method
                    , " "
                    , request.url
                    , ") to have been made, but it was not.\n"
                    , case pendingRequests of
                        [] ->
                            "    No requests were made."

                        _ ->
                            String.concat
                                [ "    The following requests were made:\n"
                                , String.join "\n" <|
                                    List.map (\( method, url ) -> "      - " ++ method ++ " " ++ url) pendingRequests
                                ]
                    ]

        Finished (EffectSimulationNotConfigured functionName) ->
            Expect.fail ("TEST SETUP ERROR: In order to use " ++ functionName ++ ", you MUST use TestContext.withSimulatedEffects before calling TestContext.start")

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

If you are writing a convenience function that is creating a test context, see [`createFailed`](#createFailed).

-}
fail : String -> String -> TestContext msg model effect -> TestContext msg model effect
fail assertionName failureMessage testContext =
    case testContext of
        Finished err ->
            Finished err

        Active _ ->
            Finished (CustomFailure assertionName failureMessage)


{-| `createFailed` can be used to report custom errors if you are writing your own convenience functions to **create** test contexts.

The parameters are:

1.  The name of your helper function (displayed in failure messages)
2.  The failure message (also included in the failure message)

NOTE: if you are writing a convenience function that takes a `TestContext` as input, you should use [`fail`](#fail) instead,
as it provides more context in the test failure message.

    -- JsonSchema and MyProgram are imaginary modules for this example


    import JsonSchema exposing (Schema, validateJsonSchema)
    import MyProgram exposing (Model, Msg)
    import TextContext exposing (TestContext)

    createWithValidatedJson : Schema -> String -> TestContext Msg Model (Cmd Msg)
    createWithValidatedJson schema json =
        case validateJsonSchema schema json of
            Err message ->
                TestContext.createFailed
                    "createWithValidatedJson"
                    ("JSON schema validation failed:\n" ++ message)

            Ok () ->
                TestContext.createElement
                    { init = MyProgram.init
                    , update = MyProgram.update
                    , view = MyProgram.view
                    }
                    |> TestContext.start json

-}
createFailed : String -> String -> TestContext msg model effect
createFailed functionName failureMessage =
    Finished (CustomFailure functionName failureMessage)
