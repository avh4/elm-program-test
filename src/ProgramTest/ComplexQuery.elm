module ProgramTest.ComplexQuery exposing (ComplexQuery, Failure(..), FailureContext(..), Highlight, Priority, andThen, check, exactlyOneOf, extractFromContext, find, findButNot, map, run, simulate, succeed)

import Expect exposing (Expectation)
import Json.Encode as Json
import ProgramTest.TestHtmlHacks as TestHtmlHacks
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Runner


type ComplexQuery msg a
    = Done a
    | Find (Maybe String) Highlight (List Selector) (Query.Single msg) (Query.Single msg -> ComplexQuery msg a)
    | FindButNot
        (Maybe String)
        Highlight
        { good : List Selector
        , bads : List (List Selector)
        , onError : List Selector
        }
        (Query.Single msg)
        (Query.Single msg -> ComplexQuery msg a)
    | Simulate ( String, Json.Value ) (Query.Single msg) (msg -> ComplexQuery msg a)
    | Check String (ComplexQuery msg ()) (ComplexQuery msg a)
    | ExactlyOneOf String (List ( String, ComplexQuery msg a ))


succeed : a -> ComplexQuery msg a
succeed =
    Done


map : (a -> b) -> ComplexQuery msg a -> ComplexQuery msg b
map f complexQuery =
    andThen (f >> Done) complexQuery


find : Maybe String -> Highlight -> List Selector -> Query.Single msg -> ComplexQuery msg (Query.Single msg)
find description highlight selectors source =
    Find description highlight selectors source Done


{-|

  - `good`: the primary selector that must match
  - `bads`: a list of selectors that must NOT match
  - `onError`: the selector to use to produce an error message if any of the checks fail

-}
findButNot :
    Maybe String
    -> Highlight
    ->
        { good : List Selector
        , bads : List (List Selector)
        , onError : List Selector
        }
    -> Query.Single msg
    -> ComplexQuery msg (Query.Single msg)
findButNot description highlight config source =
    FindButNot description highlight config source Done


exactlyOneOf : String -> List ( String, a -> ComplexQuery msg b ) -> a -> ComplexQuery msg b
exactlyOneOf description options a =
    ExactlyOneOf description
        (List.map (Tuple.mapSecond (\f -> f a)) options)


simulate : ( String, Json.Value ) -> Query.Single msg -> ComplexQuery msg msg
simulate event target =
    Simulate event target Done


{-| Ensure that the given query succeeds, but then ignore its result.
-}
check : String -> (a -> ComplexQuery msg ignored) -> ComplexQuery msg a -> ComplexQuery msg a
check description checkQuery mainQuery =
    mainQuery |> andThen (\a -> Check description (checkQuery a |> map (\_ -> ())) (Done a))


andThen : (a -> ComplexQuery msg b) -> ComplexQuery msg a -> ComplexQuery msg b
andThen f queryChain =
    case queryChain of
        Done a ->
            f a

        FindButNot description highlight config source next ->
            FindButNot description highlight config source (next >> andThen f)

        Find description highlight selectors source next ->
            Find description highlight selectors source (next >> andThen f)

        Simulate event target next ->
            Simulate event target (next >> andThen f)

        Check description checkQuery next ->
            Check description checkQuery (next |> andThen f)

        ExactlyOneOf desc options ->
            ExactlyOneOf desc (List.map (Tuple.mapSecond (andThen f)) options)


type alias Priority =
    Int


type alias State =
    { priority : Priority
    }


type Failure
    = QueryFailed TestHtmlHacks.FailureReason
    | SimulateFailed String
    | NoMatches String (List ( String, Priority, FailureContext Failure ))
    | TooManyMatches String (List ( String, FailureContext () ))


type FailureContext a
    = FindSucceeded (Maybe String) (() -> List String) (FailureContext a)
    | CheckSucceeded String (FailureContext ()) (FailureContext a)
    | Description (Result String String) (FailureContext a)
    | None a


mapFailureContext : (a -> b) -> FailureContext a -> FailureContext b
mapFailureContext f failureContext =
    case failureContext of
        FindSucceeded description selectors next ->
            FindSucceeded description selectors (mapFailureContext f next)

        CheckSucceeded description checkContext next ->
            CheckSucceeded description checkContext (mapFailureContext f next)

        Description description next ->
            Description description (mapFailureContext f next)

        None a ->
            None (f a)


extractFromContext : FailureContext a -> a
extractFromContext failureContext =
    case failureContext of
        FindSucceeded _ _ next ->
            extractFromContext next

        CheckSucceeded _ _ next ->
            extractFromContext next

        Description _ next ->
            extractFromContext next

        None a ->
            a


destructureContext : FailureContext a -> ( FailureContext (), a )
destructureContext failureContext =
    case failureContext of
        FindSucceeded description selectors baseFailure ->
            destructureContext baseFailure
                |> Tuple.mapFirst (FindSucceeded description selectors)

        CheckSucceeded description checkContext baseFailure ->
            destructureContext baseFailure
                |> Tuple.mapFirst (CheckSucceeded description checkContext)

        Description string baseFailure ->
            destructureContext baseFailure
                |> Tuple.mapFirst (Description string)

        None a ->
            ( None (), a )


type alias Highlight =
    List String


run : ComplexQuery msg a -> Result ( Highlight, FailureContext Failure ) a
run complexQuery =
    let
        ( _, ( highlight, result ) ) =
            step
                { priority = 0
                }
                complexQuery
    in
    case destructureContext result of
        ( _, Ok a ) ->
            Ok a

        ( context, Err error ) ->
            Err ( highlight, mapFailureContext (\_ -> error) context )


step : State -> ComplexQuery msg a -> ( State, ( Highlight, FailureContext (Result Failure a) ) )
step state complexQuery =
    case complexQuery of
        Done a ->
            ( state, ( [], None (Ok a) ) )

        Find description highlight selectors source next ->
            case Test.Runner.getFailureReason (Query.has [ Selector.all selectors ] source) of
                Just _ ->
                    let
                        error =
                            firstErrorOf source
                                [ Query.has selectors
                                , Query.has [ Selector.all selectors ]
                                ]

                        addDescription =
                            case description of
                                Nothing ->
                                    identity

                                Just desc ->
                                    Description (Err desc)
                    in
                    ( { state
                        | priority = state.priority + countSuccesses error
                      }
                    , ( highlight, None (Err (QueryFailed error)) )
                    )
                        |> Tuple.mapSecond (Tuple.mapSecond addDescription)

                Nothing ->
                    step
                        { state
                            | priority = state.priority + List.length selectors
                        }
                        (next (Query.find selectors source))
                        |> Tuple.mapSecond (Tuple.mapBoth ((++) highlight) (FindSucceeded description (\() -> TestHtmlHacks.getPassingSelectors selectors source)))

        ExactlyOneOf description options ->
            let
                results : List ( String, ( State, ( Highlight, FailureContext (Result Failure a) ) ) )
                results =
                    List.map (Tuple.mapSecond (step state)) options

                successes : List ( String, ( State, ( Highlight, FailureContext (Result Failure a) ) ) )
                successes =
                    List.filterMap checkSuccess results

                checkSuccess ( desc, ( newState, ( highlight, result ) ) ) =
                    case extractFromContext result of
                        Ok a ->
                            Just ( desc, ( newState, ( highlight, result ) ) )

                        Err _ ->
                            Nothing

                collectError ( desc, ( newState, ( highlight, result ) ) ) =
                    case extractFromContext result of
                        Ok _ ->
                            Nothing

                        Err x ->
                            Just
                                ( desc
                                , newState.priority
                                , mapFailureContext (\_ -> x) result
                                )
            in
            case successes of
                [ ( _, one ) ] ->
                    one

                [] ->
                    let
                        failures =
                            List.filterMap collectError results

                        highlights =
                            List.concatMap (Tuple.second >> Tuple.second >> Tuple.first) results
                    in
                    ( state, ( highlights, None (Err (NoMatches description failures)) ) )

                many ->
                    let
                        failures =
                            List.map (Tuple.mapSecond (Tuple.second >> Tuple.second >> mapFailureContext (\_ -> ()))) many

                        highlights =
                            List.concatMap (Tuple.second >> Tuple.second >> Tuple.first) many
                    in
                    ( state, ( highlights, None (Err (TooManyMatches description failures)) ) )

        FindButNot description highlight { good, bads, onError } source next ->
            -- This is tricky because Test.Html doesn't provide a way to search for an attribute being *not* present.
            -- So we have to check if a selector we don't want *is* present, and manually force a failure if it is.
            let
                addDescription =
                    case description of
                        Nothing ->
                            identity

                        Just desc ->
                            Description (Err desc)

                checkBads : Priority -> List (List Selector) -> Query.Single msg -> ( State, ( Highlight, FailureContext (Result Failure a) ) )
                checkBads extraPriority bads_ found =
                    case bads_ of
                        [] ->
                            step
                                { state | priority = state.priority + extraPriority + 1 }
                                (next found)
                                -- TODO: add the not bads to the context (or alternatively, add the "onErrors", but convert them all to successes)
                                |> Tuple.mapSecond (Tuple.mapBoth ((++) highlight) (FindSucceeded description (\() -> TestHtmlHacks.getPassingSelectors good source)))

                        nextBad :: rest ->
                            let
                                isBad =
                                    Query.has [ Selector.all nextBad ] source
                            in
                            case Test.Runner.getFailureReason isBad of
                                Nothing ->
                                    -- the element matches the bad selectors; produce a Query using the onError selectors that will fail that will show a reasonable failure message
                                    let
                                        error =
                                            firstErrorOf source
                                                [ Query.has good
                                                , Query.has [ Selector.all good ]
                                                , Query.has onError
                                                , Query.has [ Selector.all onError ]
                                                ]
                                    in
                                    ( { state | priority = state.priority + extraPriority + countSuccesses error }
                                    , ( highlight, None (Err (QueryFailed error)) )
                                    )
                                        |> Tuple.mapSecond (Tuple.mapSecond addDescription)

                                Just _ ->
                                    -- the element we found is not bad; continue on to the next check
                                    checkBads (extraPriority + List.length nextBad) rest found

                isGood =
                    Query.has [ Selector.all good ] source
            in
            case Test.Runner.getFailureReason isGood of
                Just _ ->
                    -- Couldn't find it, so report the best error message we can
                    let
                        error =
                            firstErrorOf source
                                [ Query.has good
                                , Query.has [ Selector.all good ]
                                ]
                    in
                    ( { state | priority = state.priority + countSuccesses error }
                    , ( highlight, None (Err (QueryFailed error)) )
                    )

                Nothing ->
                    Query.find good source
                        |> checkBads (List.length good) bads

        Simulate event source next ->
            case
                -- This check is maybe not needed anymore since the previous finds should all short circuit their errors?
                source
                    |> Query.has []
                    |> Test.Runner.getFailureReason
            of
                Just reason ->
                    --( state, Err (QueryFailed (TestHtmlHacks.parseFailureReason reason.description)) )
                    ( state, ( [], None (Err (QueryFailed (TestHtmlHacks.parseFailureReason "XXX: Does this code ever run????"))) ) )

                Nothing ->
                    -- Try to simulate the event, now that we know the target exists
                    case
                        source
                            |> Test.Html.Event.simulate event
                            |> Test.Html.Event.toResult
                    of
                        Err message ->
                            ( state, ( [], None (Err (SimulateFailed (TestHtmlHacks.parseSimulateFailure message))) ) )
                                |> Tuple.mapSecond (Tuple.mapSecond (Description (Err ("simulate " ++ Tuple.first event))))

                        Ok msg ->
                            step state (next msg)

        Check description checkQuery next ->
            let
                ( checkedState, ( highlight, checkResult ) ) =
                    step_ state checkQuery
            in
            case destructureContext checkResult of
                ( checkContext, Err failure ) ->
                    ( checkedState
                    , ( highlight, mapFailureContext (\() -> Err failure) checkContext )
                    )
                        |> Tuple.mapSecond (Tuple.mapSecond (Description (Err description)))

                ( checkContext, Ok () ) ->
                    step { state | priority = checkedState.priority } next
                        |> Tuple.mapSecond (Tuple.mapBoth ((++) highlight) (CheckSucceeded description checkContext))


{-| This is needed because Elm does not allow a function to call itself with a different concrete type, and we need to do so above in the `Check` branch.
-}
step_ : State -> ComplexQuery msg a -> ( State, ( Highlight, FailureContext (Result Failure a) ) )
step_ =
    step


firstErrorOf : Query.Single msg -> List (Query.Single msg -> Expectation) -> TestHtmlHacks.FailureReason
firstErrorOf source choices =
    case choices of
        [] ->
            TestHtmlHacks.parseFailureReason "PLEASE REPORT THIS AT <https://github.com/avh4/elm-program-test/issues>: firstErrorOf: asked to report an error but none of the choices failed"

        next :: rest ->
            case Test.Runner.getFailureReason (next source) of
                Just reason ->
                    TestHtmlHacks.parseFailureReason reason.description

                Nothing ->
                    firstErrorOf source rest


countSuccesses : TestHtmlHacks.FailureReason -> Int
countSuccesses failureReason =
    case failureReason of
        TestHtmlHacks.Simple string ->
            0

        TestHtmlHacks.SelectorsFailed results ->
            List.length (List.filter isOk results)


isOk : Result x a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
