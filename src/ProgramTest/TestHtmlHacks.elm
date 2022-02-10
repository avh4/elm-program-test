module ProgramTest.TestHtmlHacks exposing (FailureReason(..), getPassingSelectors, parseFailureReason, parseSimulateFailure, renderHtml)

import Html.Parser
import ProgramTest.HtmlHighlighter as HtmlHighlighter
import ProgramTest.HtmlParserHacks as HtmlParserHacks
import ProgramTest.HtmlRenderer as HtmlRenderer
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Test.Runner


pleaseReport description =
    "PLEASE REPORT THIS AT <https://github.com/avh4/elm-program-test/issues>: " ++ description


renderHtml : (String -> String) -> (String -> List Html.Parser.Attribute -> List Html.Parser.Node -> Bool) -> Query.Single any -> String
renderHtml colorHidden highlightPredicate single =
    case forceFailureReport [] single of
        ( "▼ Query.fromHtml", UnparsedSection a ) :: _ ->
            "▼ Query.fromHtml\n\n" ++ a

        ( "▼ Query.fromHtml", HtmlSection nodes ) :: _ ->
            let
                tryHighlight =
                    List.map
                        (HtmlHighlighter.highlight highlightPredicate)
                        nodes

                finalHighlighted =
                    if List.any HtmlHighlighter.isNonHiddenElement tryHighlight then
                        tryHighlight

                    else
                        List.map
                            (HtmlHighlighter.highlight (\_ _ _ -> True))
                            nodes
            in
            "▼ Query.fromHtml\n\n"
                ++ HtmlRenderer.render colorHidden 4 finalHighlighted

        ( first, _ ) :: _ ->
            pleaseReport ("unexpected failure report" ++ first)

        [] ->
            pleaseReport "renderHtml: empty failure report"


getPassingSelectors : List Selector -> Query.Single msg -> List String
getPassingSelectors selectors single =
    case List.reverse (forceFailureReport selectors single) of
        ( _, FailureSection (SelectorsFailed results) ) :: _ ->
            case List.reverse results of
                (Ok _) :: _ ->
                    [ pleaseReport "getPassingSelectors: forced selector didn't fail" ]

                _ ->
                    List.filterMap Result.toMaybe results

        ( _, FailureSection _ ) :: _ ->
            [ pleaseReport "getPassingSelectors: failure section didn't have selectors" ]

        _ ->
            [ pleaseReport "getPassingSelectors: no failure section" ]


forceFailureReport : List Selector -> Query.Single any -> List ( String, Section )
forceFailureReport selectors =
    forceFailureReport_ selectors "ProgramTest.TestHtmlHacks is trying to force a failure to collect the error message %%"


forceFailureReport_ : List Selector -> String -> Query.Single any -> List ( String, Section )
forceFailureReport_ selectors unique single =
    case
        single
            |> Query.has (selectors ++ [ Selector.text unique ])
            |> Test.Runner.getFailureReason
    of
        Nothing ->
            -- We expect the fake query to fail -- if it doesn't for some reason, just try recursing with a different fake matching string until it does fail
            forceFailureReport_ selectors (unique ++ "_") single

        Just reason ->
            parseFailureReport reason.description


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
    | HtmlSection (List Html.Parser.Node)
    | FailureSection FailureReason


parseSection : List String -> Section
parseSection lines =
    case List.filterMap parseSelectorResult lines of
        [] ->
            case HtmlParserHacks.parse (String.join "\n" lines) of
                Ok node ->
                    HtmlSection node

                Err err ->
                    UnparsedSection (String.join "\n" lines)

        some ->
            FailureSection (SelectorsFailed some)


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
