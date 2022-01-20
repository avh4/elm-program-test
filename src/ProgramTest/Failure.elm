module ProgramTest.Failure exposing (Failure(..), toString)

import Html exposing (Html)
import ProgramTest.ComplexQuery as ComplexQuery exposing (Failure(..))
import ProgramTest.TestHtmlHacks as TestHtmlHacks
import String.Extra
import Test.Html.Query as Query
import Test.Runner.Failure
import Url exposing (Url)


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
    | NoMatchingHttpRequest Int Int String { method : String, url : String } (List ( String, String ))
    | MultipleMatchingHttpRequest Int Int String { method : String, url : String } (List ( String, String ))
    | EffectSimulationNotConfigured String
    | ViewAssertionFailed String (Html ()) ComplexQuery.Failure
    | CustomFailure String String


toString : Failure -> String
toString failure =
    case failure of
        ChangedPage cause finalLocation ->
            cause ++ " caused the program to end by navigating to " ++ String.Extra.escape (Url.toString finalLocation) ++ ".  NOTE: If this is what you intended, use ProgramTest.expectPageChange to end your test."

        ExpectFailed expectationName description reason ->
            expectationName ++ ":\n" ++ Test.Runner.Failure.format description reason

        SimulateFailed functionName message ->
            functionName ++ ":\n" ++ message

        SimulateFailedToFindTarget functionName message ->
            functionName ++ ":\n" ++ message

        SimulateLastEffectFailed message ->
            "simulateLastEffect failed: " ++ message

        InvalidLocationUrl functionName invalidUrl ->
            functionName ++ ": " ++ "Not a valid absolute URL:\n" ++ String.Extra.escape invalidUrl

        InvalidFlags functionName message ->
            functionName ++ ":\n" ++ message

        ProgramDoesNotSupportNavigation functionName ->
            functionName ++ ": Program does not support navigation.  Use ProgramTest.createApplication to create a ProgramTest that supports navigation."

        NoBaseUrl functionName relativeUrl ->
            functionName ++ ": The ProgramTest does not have a base URL and cannot resolve the relative URL " ++ String.Extra.escape relativeUrl ++ ".  Use ProgramTest.withBaseUrl before calling ProgramTest.start to create a ProgramTest that can resolve relative URLs."

        NoMatchingHttpRequest expected actual functionName request pendingRequests ->
            String.concat
                [ functionName
                , ": "
                , "Expected "
                , case expected of
                    1 ->
                        "HTTP request"

                    _ ->
                        "at least " ++ String.fromInt expected ++ " HTTP requests"
                , " ("
                , request.method
                , " "
                , request.url
                , ") to have been made and still be pending, "
                , case actual of
                    0 ->
                        "but no such requests were made."

                    _ ->
                        "but only " ++ String.fromInt actual ++ " such requests were made."
                , "\n"
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

        MultipleMatchingHttpRequest expected actual functionName request pendingRequests ->
            String.concat
                [ functionName
                , ": "
                , "Expected "
                , case expected of
                    1 ->
                        "a single HTTP request"

                    _ ->
                        String.fromInt expected ++ " HTTP requests"
                , " ("
                , request.method
                , " "
                , request.url
                , ") to have been made, but "
                , String.fromInt actual
                , " such requests were made.\n"
                , case pendingRequests of
                    [] ->
                        "    No requests were made."

                    _ ->
                        String.concat
                            [ "    The following requests were made:\n"
                            , String.join "\n" <|
                                List.map (\( method, url ) -> "      - " ++ method ++ " " ++ url) pendingRequests
                            ]
                , if expected == 1 && actual > 1 then
                    let
                        useInstead =
                            if String.startsWith "simulate" functionName then
                                "simulateHttpResponseAdvanced"

                            else if String.startsWith "expect" functionName then
                                "expectHttpRequests"

                            else
                                "ensureHttpRequests"
                    in
                    "\n\nNOTE: If you want to allow multiple requests to the same endpoint, use ProgramTest." ++ useInstead ++ "."

                  else
                    ""
                ]

        EffectSimulationNotConfigured functionName ->
            "TEST SETUP ERROR: In order to use " ++ functionName ++ ", you MUST use ProgramTest.withSimulatedEffects before calling ProgramTest.start"

        ViewAssertionFailed functionName html reason ->
            String.join "\n"
                [ TestHtmlHacks.renderHtml (Query.fromHtml html)
                , "▼ " ++ functionName
                , ""
                , renderQueryFailure 0 True reason
                ]

        CustomFailure assertionName message ->
            assertionName ++ ": " ++ message


renderQueryFailure : Int -> Bool -> ComplexQuery.Failure -> String
renderQueryFailure indent color failure =
    let
        indentS =
            String.repeat indent " "
    in
    case failure of
        QueryFailed failureReason ->
            renderTestHtmlFailureReason indent (colorsFor color) failureReason

        ComplexQuery.SimulateFailed string ->
            let
                colors =
                    colorsFor color
            in
            indentS ++ colors.bold string

        NoMatches description options ->
            let
                sortedByPriority =
                    options
                        |> List.sortBy (\( _, prio, _ ) -> -prio)

                maxPriority =
                    List.head sortedByPriority
                        |> Maybe.map (\( _, prio, _ ) -> prio)
                        |> Maybe.withDefault 0
            in
            String.join "\n" <|
                List.concat
                    [ [ indentS ++ description ++ ":" ]
                    , sortedByPriority
                        |> List.filter (\( _, prio, _ ) -> prio > maxPriority - 2)
                        |> List.map (\( desc, prio, reason ) -> indentS ++ "- " ++ desc ++ "\n" ++ renderQueryFailure (indent + 4) (color && prio >= maxPriority - 1) reason)
                    ]

        TooManyMatches description matches ->
            String.join "\n" <|
                List.concat
                    [ [ indentS ++ description ++ ", but there were multiple successful matches:" ]
                    , List.map (\desc -> indentS ++ "- " ++ desc) matches
                    , [ ""
                      , "If that's what you intended, use `ProgramTest.within` to focus in on a portion of"
                      , "the view that contains only one of the matches."
                      ]
                    ]

        FindSucceeded successfulChecks baseFailure ->
            String.join "\n"
                [ renderSelectorResults indent (colorsFor color) (List.map Ok successfulChecks)
                , renderQueryFailure (indent + 2) color baseFailure
                ]


renderTestHtmlFailureReason : Int -> Colors -> TestHtmlHacks.FailureReason -> String
renderTestHtmlFailureReason indent colors failureReason =
    let
        indentS =
            String.repeat indent " "
    in
    case failureReason of
        TestHtmlHacks.Simple string ->
            indentS ++ string

        TestHtmlHacks.SelectorsFailed results ->
            renderSelectorResults indent colors results


renderSelectorResults : Int -> Colors -> List (Result String String) -> String
renderSelectorResults indent colors results =
    let
        indentS =
            String.repeat indent " "
    in
    List.map ((++) indentS << renderSelectorResult colors) (upToFirstErr results)
        |> String.join "\n"


renderSelectorResult : Colors -> Result String String -> String
renderSelectorResult colors result =
    case result of
        Ok selector ->
            String.concat
                [ colors.green "✓"
                , " "
                , colors.bold selector
                ]

        Err selector ->
            colors.red <|
                String.concat
                    [ "✗"
                    , " "
                    , selector
                    ]


upToFirstErr : List (Result x a) -> List (Result x a)
upToFirstErr results =
    let
        step acc results_ =
            case results_ of
                [] ->
                    acc

                (Err x) :: _ ->
                    Err x :: acc

                (Ok a) :: rest ->
                    step (Ok a :: acc) rest
    in
    step [] results
        |> List.reverse


type alias Colors =
    { bold : String -> String
    , red : String -> String
    , green : String -> String
    }


colorsFor : Bool -> Colors
colorsFor show =
    if show then
        showColors

    else
        noColors


showColors : Colors
showColors =
    { bold = \s -> String.concat [ "\u{001B}[1m", s, "\u{001B}[22m" ]
    , red = \s -> String.concat [ "\u{001B}[31m", s, "\u{001B}[39m" ]
    , green = \s -> String.concat [ "\u{001B}[32m", s, "\u{001B}[39m" ]
    }


noColors : Colors
noColors =
    { bold = identity, red = identity, green = identity }
