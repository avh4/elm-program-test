module ProgramTest.TestHtmlHacks exposing (FailureReason(..), parseFailureReason, parseSimulateFailure)


type FailureReason
    = Simple String
    | SelectorsFailed (List (Result String String))


parseFailureReason : String -> FailureReason
parseFailureReason string =
    let
        lines =
            String.lines string
    in
    case List.filterMap parseSelectorResult lines of
        [] ->
            Simple ("PLEASE REPORT THIS AT <https://github.com/avh4/elm-program-test/issues>: Got a failure message from Test.Html.Query that we couldn't parse: " ++ string)

        some ->
            SelectorsFailed some


parseSelectorResult : String -> Maybe (Result String String)
parseSelectorResult string =
    case String.left 2 string of
        "✓ " ->
            Just (Ok (String.trimRight (String.dropLeft 2 string)))

        "✗ " ->
            Just (Err (String.trimRight (String.dropLeft 2 string)))

        _ ->
            Nothing


parseSimulateFailure : String -> String
parseSimulateFailure string =
    String.lines string
        |> List.head
        |> Maybe.withDefault string
