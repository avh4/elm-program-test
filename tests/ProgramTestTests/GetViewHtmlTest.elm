module ProgramTestTests.GetViewHtmlTest exposing (all)

import Expect exposing (Expectation)
import Html
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Selector as Selector


type alias Model =
    { count : Int }


type Msg
    = Increment
    | Decrement


start : ProgramTest Model Msg ()
start =
    ProgramTest.createSandbox
        { init = { count = 0 }
        , update =
            \msg model ->
                case msg of
                    Increment ->
                        { model | count = model.count + 1 }

                    Decrement ->
                        { model | count = model.count - 1 }
        , view =
            \model ->
                Html.div [ class "counter" ]
                    [ Html.button [ onClick Decrement ] [ Html.text "-" ]
                    , Html.span [] [ Html.text (String.fromInt model.count) ]
                    , Html.button [ onClick Increment ] [ Html.text "+" ]
                    ]
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "getViewHtml"
        [ describe "with empty selector list (full view)"
            [ test "returns the rendered HTML of the initial view" <|
                \() ->
                    start
                        |> ProgramTest.getViewHtml []
                        |> expectOkHtml """
<div class="counter">
    <button>
        -
    </button>
    <span>
        0
    </span>
    <button>
        +
    </button>
</div>
"""
            , test "returns the rendered HTML after interactions" <|
                \() ->
                    start
                        |> ProgramTest.clickButton "+"
                        |> ProgramTest.clickButton "+"
                        |> ProgramTest.clickButton "+"
                        |> ProgramTest.getViewHtml []
                        |> Result.map (String.contains "3")
                        |> Expect.equal (Ok True)
            , test "returns Err when the ProgramTest is in a failed state" <|
                \() ->
                    start
                        |> ProgramTest.clickButton "nonexistent button"
                        |> ProgramTest.getViewHtml []
                        |> isErr
                        |> Expect.equal True
            , test "error message describes the original failure" <|
                \() ->
                    start
                        |> ProgramTest.clickButton "nonexistent button"
                        |> ProgramTest.getViewHtml []
                        |> Result.mapError (String.contains "nonexistent button")
                        |> Expect.equal (Err True)
            , test "returns Err for a program that failed to create" <|
                \() ->
                    ProgramTest.createFailed "setup" "bad config"
                        |> ProgramTest.getViewHtml []
                        |> isErr
                        |> Expect.equal True
            , test "works with a simple view" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view = \() -> Html.p [] [ Html.text "hello" ]
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.getViewHtml []
                        |> expectOkHtml """
<p>
    hello
</p>
"""
            , test "works with nested elements and attributes" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view =
                            \() ->
                                Html.div []
                                    [ Html.a [ Html.Attributes.href "/home" ] [ Html.text "Home" ]
                                    ]
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.getViewHtml []
                        |> Result.map (String.contains "href=\"/home\"")
                        |> Expect.equal (Ok True)
            ]
        , describe "with selectors"
            [ test "returns only the matching element" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view =
                            \() ->
                                Html.div []
                                    [ Html.header [ id "header" ] [ Html.text "Header" ]
                                    , Html.main_ [ id "content" ]
                                        [ Html.p [] [ Html.text "Main content" ]
                                        ]
                                    , Html.footer [ id "footer" ] [ Html.text "Footer" ]
                                    ]
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.getViewHtml [ Selector.id "content" ]
                        |> expectOkHtml """
<main id="content">
    <p>
        Main content
    </p>
</main>
"""
            , test "works with tag selector" <|
                \() ->
                    start
                        |> ProgramTest.getViewHtml [ Selector.tag "span" ]
                        |> expectOkHtml """
<span>
    0
</span>
"""
            , test "works with class selector" <|
                \() ->
                    ProgramTest.createSandbox
                        { init = ()
                        , update = \() () -> ()
                        , view =
                            \() ->
                                Html.div []
                                    [ Html.header [] [ Html.text "Header" ]
                                    , Html.div [ class "content" ]
                                        [ Html.p [] [ Html.text "Main content" ]
                                        ]
                                    ]
                        }
                        |> ProgramTest.start ()
                        |> ProgramTest.getViewHtml [ Selector.class "content" ]
                        |> expectOkHtml """
<div class="content">
    <p>
        Main content
    </p>
</div>
"""
            , test "returns Err when selector matches nothing" <|
                \() ->
                    start
                        |> ProgramTest.getViewHtml [ Selector.id "nonexistent" ]
                        |> isErr
                        |> Expect.equal True
            , test "returns Err when selector matches multiple elements" <|
                \() ->
                    start
                        |> ProgramTest.getViewHtml [ Selector.tag "button" ]
                        |> isErr
                        |> Expect.equal True
            ]
        ]


expectOkHtml : String -> Result String String -> Expectation
expectOkHtml expected actual =
    actual
        |> Expect.equal (Ok (String.trim expected))


isErr : Result a b -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False
