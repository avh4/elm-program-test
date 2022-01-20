module ProgramTest.ComplexQuery exposing (ComplexQuery, Failure(..), Priority, andThen, check, exactlyOneOf, find, findButNot, map, run, simulate, succeed)

import Expect exposing (Expectation)
import Json.Encode as Json
import ProgramTest.TestHtmlHacks as TestHtmlHacks
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Runner


type ComplexQuery msg a
    = Done a
    | Find (List Selector) (Query.Single msg) (Query.Single msg -> ComplexQuery msg a)
    | FindButNot
        { good : List Selector
        , bads : List (List Selector)
        , onError : List Selector
        }
        (Query.Single msg)
        (Query.Single msg -> ComplexQuery msg a)
    | Simulate ( String, Json.Value ) (Query.Single msg) (msg -> ComplexQuery msg a)
    | Check (ComplexQuery msg ()) (ComplexQuery msg a)
    | ExactlyOneOf String (List ( String, ComplexQuery msg a ))


succeed : a -> ComplexQuery msg a
succeed =
    Done


map : (a -> b) -> ComplexQuery msg a -> ComplexQuery msg b
map f complexQuery =
    andThen (f >> Done) complexQuery


find : List Selector -> Query.Single msg -> ComplexQuery msg (Query.Single msg)
find selectors source =
    Find selectors source Done


{-|

  - `good`: the primary selector that must match
  - `bads`: a list of selectors that must NOT match
  - `onError`: the selector to use to produce an error message if any of the checks fail

-}
findButNot :
    { good : List Selector
    , bads : List (List Selector)
    , onError : List Selector
    }
    -> Query.Single msg
    -> ComplexQuery msg (Query.Single msg)
findButNot config source =
    FindButNot config source Done


exactlyOneOf : String -> List ( String, a -> ComplexQuery msg b ) -> a -> ComplexQuery msg b
exactlyOneOf description options a =
    ExactlyOneOf description
        (List.map (Tuple.mapSecond (\f -> f a)) options)


simulate : ( String, Json.Value ) -> Query.Single msg -> ComplexQuery msg msg
simulate event target =
    Simulate event target Done


{-| Ensure that the given query succeeds, but then ignore its result.
-}
check : (a -> ComplexQuery msg ignored) -> ComplexQuery msg a -> ComplexQuery msg a
check checkQuery mainQuery =
    Check (mainQuery |> andThen checkQuery |> map (\_ -> ())) mainQuery


andThen : (a -> ComplexQuery msg b) -> ComplexQuery msg a -> ComplexQuery msg b
andThen f queryChain =
    case queryChain of
        Done a ->
            f a

        FindButNot config source next ->
            FindButNot config source (next >> andThen f)

        Find selectors source next ->
            Find selectors source (next >> andThen f)

        Simulate event target next ->
            Simulate event target (next >> andThen f)

        Check checkQuery next ->
            Check checkQuery (next |> andThen f)

        ExactlyOneOf desc options ->
            ExactlyOneOf desc (List.map (Tuple.mapSecond (andThen f)) options)


type alias Priority =
    Int


type alias State =
    { priority : Priority
    , errorContext : Failure -> Failure
    }


type Failure
    = -- base failures
      QueryFailed TestHtmlHacks.FailureReason
    | SimulateFailed String
    | NoMatches String (List ( String, Priority, Failure ))
    | TooManyMatches String (List String)
      -- Extra information about the context of a failure
    | FindSucceeded (List String) Failure


run : ComplexQuery msg a -> Result Failure a
run complexQuery =
    let
        ( finalState, result ) =
            step
                { priority = 0
                , errorContext = identity
                }
                complexQuery
    in
    Result.mapError finalState.errorContext result


step : State -> ComplexQuery msg a -> ( State, Result Failure a )
step state complexQuery =
    case complexQuery of
        Done a ->
            ( state, Ok a )

        Find selectors source next ->
            case Test.Runner.getFailureReason (Query.has [ Selector.all selectors ] source) of
                Just _ ->
                    let
                        error =
                            firstErrorOf source
                                [ Query.has selectors
                                , Query.has [ Selector.all selectors ]
                                ]
                    in
                    ( { state
                        | priority = state.priority + countSuccesses error
                      }
                    , Err (QueryFailed error)
                    )

                Nothing ->
                    step
                        { state
                            | priority = state.priority + List.length selectors
                            , errorContext =
                                state.errorContext
                                    << FindSucceeded
                                        (TestHtmlHacks.getPassingSelectors selectors source)
                        }
                        (next (Query.find selectors source))

        ExactlyOneOf description options ->
            let
                results =
                    List.map (Tuple.mapSecond (step state)) options

                successes =
                    List.filterMap checkSuccess results

                checkSuccess ( desc, ( newState, result ) ) =
                    case result of
                        Ok a ->
                            Just ( desc, ( newState, Ok a ) )

                        Err _ ->
                            Nothing

                collectError ( desc, ( newState, result ) ) =
                    case result of
                        Ok _ ->
                            Nothing

                        Err x ->
                            Just ( desc, newState.priority, newState.errorContext x )
            in
            case successes of
                [ ( _, one ) ] ->
                    one

                [] ->
                    ( state, Err (NoMatches description (List.filterMap collectError results)) )

                many ->
                    ( state, Err (TooManyMatches description (List.map Tuple.first many)) )

        FindButNot { good, bads, onError } source next ->
            -- This is tricky because Test.Html doesn't provide a way to search for an attribute being *not* present.
            -- So we have to check if a selector we don't want *is* present, and manually force a failure if it is.
            let
                checkBads : Priority -> List (List Selector) -> Query.Single msg -> ( State, Result Failure a )
                checkBads extraPriority bads_ found =
                    case bads_ of
                        [] ->
                            step
                                { state | priority = state.priority + extraPriority + 1 }
                                (next found)

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
                                    , Err (QueryFailed error)
                                    )

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
                    , Err (QueryFailed error)
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
                    ( state, Err (QueryFailed (TestHtmlHacks.parseFailureReason "XXX: Does this code ever run????")) )

                Nothing ->
                    -- Try to simulate the event, now that we know the target exists
                    case
                        source
                            |> Test.Html.Event.simulate event
                            |> Test.Html.Event.toResult
                    of
                        Err message ->
                            ( state, Err (SimulateFailed (TestHtmlHacks.parseSimulateFailure message)) )

                        Ok msg ->
                            step state (next msg)

        Check checkQuery next ->
            case step_ state checkQuery of
                ( checkedState, Err failure ) ->
                    ( checkedState, Err failure )

                ( checkedState, Ok () ) ->
                    step checkedState next


{-| This is needed because Elm does not allow a function to call itself with a different concrete type, and we need to do so above in the `Check` branch.
-}
step_ : State -> ComplexQuery msg a -> ( State, Result Failure a )
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
