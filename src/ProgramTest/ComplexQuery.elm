module ProgramTest.ComplexQuery exposing (ComplexQuery, Failure(..), Priority, andThen, exactlyOneOf, find, findButNot, run, simulate)

import Expect exposing (Expectation)
import Json.Encode as Json
import ProgramTest.TestHtmlHacks as TestHtmlHacks
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Runner


type ComplexQueryF msg a
    = Find (List Selector) (Query.Single msg) (Query.Single msg -> a)
    | FindButNot
        { good : List Selector
        , bads : List (List Selector)
        , onError : List Selector
        }
        (Query.Single msg)
        (Query.Single msg -> a)
    | Simulate ( String, Json.Value ) (Query.Single msg) (msg -> a)
    | ExactlyOneOf String (List ( String, ComplexQuery msg a ))


mapF : (a -> b) -> ComplexQueryF msg a -> ComplexQueryF msg b
mapF f complexQuery =
    case complexQuery of
        FindButNot config source next ->
            FindButNot config source (next >> f)

        Find selectors source next ->
            Find selectors source (next >> f)

        Simulate event target next ->
            Simulate event target (next >> f)

        ExactlyOneOf desc options ->
            ExactlyOneOf desc (List.map (Tuple.mapSecond (map f)) options)


find : List Selector -> Query.Single msg -> ComplexQuery msg (Query.Single msg)
find selectors source =
    Continue (Find selectors source Done)


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
    Continue (FindButNot config source Done)


exactlyOneOf : String -> List ( String, ComplexQuery msg a ) -> ComplexQuery msg a
exactlyOneOf description options =
    Continue (ExactlyOneOf description (List.map (Tuple.mapSecond (map Done)) options))


simulate : ( String, Json.Value ) -> Query.Single msg -> ComplexQuery msg msg
simulate event target =
    Continue (Simulate event target Done)


andThen : (a -> ComplexQuery msg b) -> ComplexQuery msg a -> ComplexQuery msg b
andThen f queryChain =
    case queryChain of
        Done a ->
            f a

        Continue start ->
            Continue (mapF (andThen f) start)


type alias Priority =
    Int


type alias State =
    { priority : Priority
    }


type Failure
    = QueryFailed TestHtmlHacks.FailureReason
    | SimulateFailed String
    | NoMatches String (List ( String, Priority, Failure ))
    | TooManyMatches String (List String)


type ComplexQuery msg a
    = Done a
    | Continue (ComplexQueryF msg (ComplexQuery msg a))


map : (a -> b) -> ComplexQuery msg a -> ComplexQuery msg b
map f complexQuery =
    case complexQuery of
        Done a ->
            Done (f a)

        Continue next ->
            Continue (mapF (map f) next)


run : ComplexQuery msg a -> Result Failure a
run complexQuery =
    let
        ( _, result ) =
            stepChain
                { priority = 0
                }
                complexQuery
    in
    result


stepChain : State -> ComplexQuery msg a -> ( State, Result Failure a )
stepChain state query =
    case query of
        Done a ->
            ( state, Ok a )

        Continue next ->
            case step state next of
                ( newState, Err x ) ->
                    ( newState, Err x )

                ( newState, Ok nextChain ) ->
                    stepChain newState nextChain


step : State -> ComplexQueryF msg a -> ( State, Result Failure a )
step state complexQuery =
    case complexQuery of
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
                    ( { state | priority = state.priority + countSuccesses error }
                    , Err (QueryFailed error)
                    )

                Nothing ->
                    ( { state | priority = state.priority + List.length selectors }
                    , Ok (next (Query.find selectors source))
                    )

        ExactlyOneOf description options ->
            let
                results =
                    List.map (Tuple.mapSecond (stepChain state)) options

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
                            Just ( desc, newState.priority, x )
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
                            ( { state | priority = state.priority + extraPriority + 1 }, Ok (next found) )

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
                            ( state, Ok (next msg) )


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
