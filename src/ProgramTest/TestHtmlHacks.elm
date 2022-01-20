module ProgramTest.TestHtmlHacks exposing (FailureReason(..), parseFailureReason, parseSimulateFailure, renderHtml)

import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner


pleaseReport description =
    "PLEASE REPORT THIS AT <https://github.com/avh4/elm-program-test/issues>: " ++ description


renderHtml : Query.Single any -> String
renderHtml =
    renderHtml_ "ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%"


renderHtml_ : String -> Query.Single any -> String
renderHtml_ unique single =
    case
        single
            |> Query.has [ Selector.text unique ]
            |> Test.Runner.getFailureReason
    of
        Nothing ->
            -- We expect the fake query to fail -- if it doesn't for some reason, just try recursing with a different fake matching string until it does fail
            renderHtml_ (unique ++ "_") single

        Just reason ->
            case parseFailureReport reason.description of
                ( "▼ Query.fromHtml", UnparsedSection a ) :: _ ->
                    "▼ Query.fromHtml\n\n" ++ a

                ( first, _ ) :: _ ->
                    pleaseReport ("unexpected failure report" ++ first)

                [] ->
                    pleaseReport "renderHtml: empty failure report"


parseFailureReport : String -> List ( String, Section )
parseFailureReport string =
    String.lines string
        |> partitionSections
        |> List.map parseNamedSection


partitionSections : List String -> List (List String)
partitionSections lines =
    partitionSections_ [] [] lines


partitionSections_ : List String -> List (List String) -> List String -> List (List String)
partitionSections_ accLines accSections remaining =
    case remaining of
        [] ->
            case List.reverse (List.reverse accLines :: accSections) of
                [] :: rest ->
                    rest

                all ->
                    all

        next :: rest ->
            if String.startsWith "▼ " next then
                partitionSections_ [ next ] (List.reverse accLines :: accSections) rest

            else
                partitionSections_ (next :: accLines) accSections rest


parseNamedSection : List String -> ( String, Section )
parseNamedSection lines =
    case lines of
        [] ->
            ( pleaseReport "empty section", UnparsedSection (pleaseReport "empty section") )

        first :: "" :: rest ->
            ( first, parseSection rest )

        _ ->
            ( pleaseReport "parseNamedSection _", UnparsedSection (String.join "\n" lines) )


type Section
    = UnparsedSection String
    | FailureSection FailureReason


parseSection : List String -> Section
parseSection lines =
    -- TODO
    UnparsedSection (String.join "\n" lines)


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
            Simple (pleaseReport "Got a failure message from Test.Html.Query that we couldn't parse: " ++ string)

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
