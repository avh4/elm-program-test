module ProgramTest.Failure exposing (Failure(..), toString)

import String.Extra
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
    | NoMatchingHttpRequest String { method : String, url : String } (List ( String, String ))
    | MultipleMatchingHttpRequest Int String { method : String, url : String } (List ( String, String ))
    | EffectSimulationNotConfigured String
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

        NoMatchingHttpRequest functionName request pendingRequests ->
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

        MultipleMatchingHttpRequest n functionName request pendingRequests ->
            String.concat
                [ functionName
                , ": "
                , "Expected a single HTTP request ("
                , request.method
                , " "
                , request.url
                , ") to have been made, but "
                , String.fromInt n
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
                ]

        EffectSimulationNotConfigured functionName ->
            "TEST SETUP ERROR: In order to use " ++ functionName ++ ", you MUST use ProgramTest.withSimulatedEffects before calling ProgramTest.start"

        CustomFailure assertionName message ->
            assertionName ++ ": " ++ message
