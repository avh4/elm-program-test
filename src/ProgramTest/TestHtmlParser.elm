module ProgramTest.TestHtmlParser exposing (Assertion(..), FailureReport(..), Selector(..), Step(..), parser)

import Html.Parser
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra.String
import ProgramTest.HtmlParserHacks as HtmlParserHacks


type FailureReport html
    = QueryFailure html (List (Step html)) Assertion
    | EventFailure String html


type Step html
    = FindStep (List Selector) html


type Selector
    = Tag String
    | Containing (List Selector)
    | Text String
    | Attribute String String
    | All (List Selector)


type Assertion
    = Has (List Selector) (List (Result String String))


parser : Parser (FailureReport Html.Parser.Node)
parser =
    Parser.oneOf
        [ Parser.succeed QueryFailure
            |. Parser.keyword "▼ Query.fromHtml"
            |. Parser.symbol "\n\n    "
            |= trimmedHtml
            |. Parser.symbol "\n\n\n"
            |= stepsParser
            |= assertionParser
            |. Parser.end
        , Parser.succeed EventFailure
            |. Parser.keyword "Event.expectEvent:"
            |. Parser.symbol " I found a node, but it does not listen for \""
            |= (Parser.getChompedString <| Parser.chompUntil "\"")
            |. Parser.symbol "\" events like I expected it would.\n\n"
            |= trimmedHtml
            |. Parser.end
        ]


trimmedHtml : Parser Html.Parser.Node
trimmedHtml =
    Parser.map HtmlParserHacks.trimText Html.Parser.node


stepsParser : Parser (List (Step Html.Parser.Node))
stepsParser =
    Parser.loop [] <|
        \acc ->
            Parser.oneOf
                [ Parser.succeed (\stmt -> Parser.Loop (stmt :: acc))
                    |= stepParser
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse acc))
                ]


stepParser : Parser (Step Html.Parser.Node)
stepParser =
    Parser.oneOf
        [ Parser.succeed FindStep
            |. Parser.keyword "▼ Query.find "
            |= selectorsParser
            |. Parser.symbol "\n\n    1)  "
            |= trimmedHtml
            |. Parser.symbol "\n\n\n"
        ]


selectorsParser : Parser (List Selector)
selectorsParser =
    Parser.sequence
        { start = "[ "
        , separator = ", "
        , end = " ]"
        , spaces = Parser.succeed ()
        , item = selectorParser
        , trailing = Parser.Forbidden
        }


selectorParser : Parser Selector
selectorParser =
    -- As of elm-explorations/test 1.2.2, `Selector.all` renders simply as a space-separated sequence of selectors
    let
        done acc =
            case acc of
                [ single ] ->
                    single

                more ->
                    All (List.reverse more)
    in
    singleSelectorParser
        |> Parser.andThen
            (\first ->
                Parser.loop [ first ] <|
                    \acc ->
                        Parser.oneOf
                            [ Parser.succeed (\stmt -> Parser.Loop (stmt :: acc))
                                |. Parser.backtrackable (Parser.symbol " ")
                                |= singleSelectorParser
                            , Parser.succeed ()
                                |> Parser.map (\_ -> Parser.Done (done acc))
                            ]
            )


singleSelectorParser : Parser Selector
singleSelectorParser =
    Parser.oneOf
        [ Parser.succeed Tag
            |. Parser.keyword "tag "
            |= Parser.Extra.String.string
        , Parser.succeed Text
            |. Parser.keyword "text "
            |= Parser.Extra.String.string
        , Parser.succeed Attribute
            |. Parser.keyword "attribute "
            |= Parser.Extra.String.string
            |. Parser.symbol " "
            |= Parser.oneOf
                [ Parser.Extra.String.string
                , Parser.succeed "true"
                    |. Parser.keyword "True"
                , Parser.succeed "false"
                    |. Parser.keyword "False"
                ]
        , Parser.succeed Containing
            |. Parser.keyword "containing "
            |= Parser.lazy (\() -> selectorsParser)
            |. Parser.symbol " "
        ]


assertionParser : Parser Assertion
assertionParser =
    Parser.oneOf
        [ Parser.succeed Has
            |. Parser.keyword "▼ Query.has "
            |= selectorsParser
            |. Parser.symbol "\n\n"
            |= selectorResultsParser
        ]


selectorResultsParser : Parser (List (Result String String))
selectorResultsParser =
    let
        help acc =
            Parser.oneOf
                [ Parser.succeed (\stmt -> Parser.Loop (stmt :: acc))
                    |= selectorResultParser
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse acc))
                ]
    in
    Parser.loop [] help


selectorResultParser : Parser (Result String String)
selectorResultParser =
    Parser.oneOf
        [ Parser.succeed (Ok << String.trim)
            |. Parser.symbol "✓ "
            |. Parser.commit ()
            |= (Parser.getChompedString <| Parser.chompUntilEndOr "\n")
            |. Parser.chompWhile ((==) '\n')
        , Parser.succeed (Err << String.trim)
            |. Parser.symbol "✗ "
            |. Parser.commit ()
            |= (Parser.getChompedString <| Parser.chompUntilEndOr "\n")
            |. Parser.chompWhile ((==) '\n')
        ]
