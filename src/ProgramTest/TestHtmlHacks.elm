module ProgramTest.TestHtmlHacks exposing (FailureReason(..), parseFailureReason)


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
            case List.filterMap parseSimpleMessage lines of
                [ single ] ->
                    Simple single

                _ ->
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


parseSimpleMessage : String -> Maybe String
parseSimpleMessage string =
    if String.startsWith "Event.expectEvent" string then
        Just string

    else
        Nothing
