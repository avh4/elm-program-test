module TestContextTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import TestContext exposing (TestContext)


type TestEffect
    = NoOp
    | LogUpdate String


testInit : ( String, TestEffect )
testInit =
    ( "<INIT>"
    , NoOp
    )


testUpdate : String -> String -> ( String, TestEffect )
testUpdate msg model =
    ( model ++ ";" ++ msg
    , LogUpdate msg
    )


testView : String -> Html String
testView model =
    Html.div []
        [ Html.span [] [ Html.text model ]
        , Html.button [ onClick "CLICK" ] [ Html.text "Click Me" ]
        , Html.node "strange" [ Html.Events.on "odd" Json.Decode.string ] []
        ]


testContext : TestContext String String TestEffect
testContext =
    TestContext.create
        { init = testInit
        , update = testUpdate
        , view = testView
        }


all : Test
all =
    describe "TestContext"
        [ test "has initial model" <|
            \() ->
                testContext
                    |> TestContext.expectModel (Expect.equal "<INIT>")
        , test "can send a msg" <|
            \() ->
                testContext
                    |> TestContext.update "A"
                    |> TestContext.expectModel (Expect.equal "<INIT>;A")
        , test "can click a button" <|
            \() ->
                testContext
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectModel (Expect.equal "<INIT>;CLICK")
        , test "can create with flags" <|
            \() ->
                TestContext.createWithFlags
                    { init = \flags -> ( "<INIT:" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "flags"
                    |> TestContext.expectModel (Expect.equal "<INIT:flags>")
        , test "can create with JSON string flags" <|
            \() ->
                TestContext.createWithJsonStringFlags
                    (Json.Decode.field "y" Json.Decode.string)
                    { init = \flags -> ( "<INIT:" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    """{"y": "fromJson"}"""
                    |> TestContext.expectModel (Expect.equal "<INIT:fromJson>")
        , test "can create with navigation" <|
            \() ->
                TestContext.createWithNavigation
                    .pathname
                    { init = \location -> ( "<INIT:" ++ location.pathname ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "https://example.com/path"
                    |> TestContext.expectModel (Expect.equal "<INIT:/path>")
        , test "can simulate a route change" <|
            \() ->
                TestContext.createWithNavigation
                    .pathname
                    { init = \location -> ( "<INIT:" ++ location.pathname ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "https://example.com/path"
                    |> TestContext.routeChange "https://example.com/new"
                    |> TestContext.expectModel (Expect.equal "<INIT:/path>;/new")
        , test "can simulate a route change with a relative URL" <|
            \() ->
                TestContext.createWithNavigation
                    .pathname
                    { init = \location -> ( "<INIT:" ++ location.pathname ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "https://example.com/path"
                    |> TestContext.routeChange "/new"
                    |> TestContext.expectModel (Expect.equal "<INIT:/path>;/new")
        , test "can create with navigation and flags" <|
            \() ->
                TestContext.createWithNavigationAndFlags
                    .pathname
                    { init = \flags location -> ( "<INIT:" ++ location.pathname ++ ":" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "https://example.com/path"
                    "flags"
                    |> TestContext.expectModel (Expect.equal "<INIT:/path:flags>")
        , test "can assert on the view" <|
            \() ->
                testContext
                    |> TestContext.shouldHaveView
                        (Query.find [ Selector.tag "span" ] >> Query.has [ Selector.text "<INIT>" ])
                    |> TestContext.done
        , test "can assert on the view concisely with a terminal assertion" <|
            \() ->
                testContext
                    |> TestContext.expectView
                        (Query.find [ Selector.tag "span" ] >> Query.has [ Selector.text "<INIT>" ])
        , test "can create with navigation and JSON string flags" <|
            \() ->
                TestContext.createWithNavigationAndJsonStringFlags
                    (Json.Decode.field "x" Json.Decode.string)
                    .pathname
                    { init = \flags location -> ( "<INIT:" ++ location.pathname ++ ":" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    "https://example.com/path"
                    """{"x": "fromJson"}"""
                    |> TestContext.expectModel (Expect.equal "<INIT:/path:fromJson>")
        , test "can assert on the view concisely given Html.Test.Selectors" <|
            \() ->
                testContext
                    |> TestContext.shouldHave [ Selector.tag "span" ]
                    |> TestContext.done
        , test "can assert on the view concisely with a terminal assertion given Html.Test.Selectors" <|
            \() ->
                testContext
                    |> TestContext.expectViewHas [ Selector.tag "span" ]
        , test "can assert on the view concisely given Html.Test.Selectors that should not exist" <|
            \() ->
                testContext
                    |> TestContext.shouldNotHave [ Selector.tag "article" ]
                    |> TestContext.done
        , test "can simulate an arbitrary DOM event" <|
            \() ->
                testContext
                    |> TestContext.simulate
                        (Query.find [ Selector.tag "strange" ])
                        ( "odd", Json.Encode.string "<ODD-VALUE>" )
                    |> TestContext.expectModel (Expect.equal "<INIT>;<ODD-VALUE>")
        , test "can assert on the last effect after init" <|
            \() ->
                testContext
                    |> TestContext.expectLastEffect (Expect.equal NoOp)
        , test "can assert on the last effect after update" <|
            \() ->
                testContext
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.expectLastEffect (Expect.equal (LogUpdate "CLICK"))
        , test "can assert on the last effect as an intermediate assertion" <|
            \() ->
                testContext
                    |> TestContext.shouldHaveLastEffect (Expect.equal NoOp)
                    |> TestContext.clickButton "Click Me"
                    |> TestContext.shouldHaveLastEffect (Expect.equal (LogUpdate "CLICK"))
                    |> TestContext.done
        , test "can be forced into failure" <|
            \() ->
                testContext
                    |> TestContext.fail "custom" "Because I said so"
                    |> TestContext.done
                    |> Test.Runner.getFailureReason
                    |> Maybe.map .description
                    |> Expect.equal (Just "custom: Because I said so")
        ]
