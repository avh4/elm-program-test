module ProgramTestTests exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (for, id, type_)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode
import ProgramTest exposing (ProgramTest)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner


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


handleInput : String -> Html.Attribute String
handleInput fieldId =
    Html.Events.onInput (\text -> "Input:" ++ fieldId ++ ":" ++ text)


handleCheck : String -> Html.Attribute String
handleCheck fieldId =
    Html.Events.onCheck (\bool -> "Check:" ++ fieldId ++ ":" ++ boolToString bool)


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"


testView : String -> Html String
testView model =
    Html.div []
        [ Html.span [] [ Html.text model ]
        , Html.button [ onClick "CLICK" ] [ Html.text "Click Me" ]
        , Html.node "strange" [ Html.Events.on "odd" Json.Decode.string ] []
        , Html.textarea [ handleInput "textarea" ] []
        , Html.div []
            [ Html.label [ for "field-1" ] [ Html.text "Field 1" ]
            , Html.input [ id "field-1", handleInput "field-1" ] []
            , Html.label [ for "field-2" ] [ Html.text "Field 2" ]
            , Html.input [ id "field-2", handleInput "field-2" ] []
            , Html.label [ for "checkbox-1" ] [ Html.text "Checkbox 1" ]
            , Html.input [ type_ "checkbox", id "checkbox-1", handleCheck "checkbox-1" ] []
            ]
        , Html.div []
            [ Html.div [ id "button-a" ]
                [ Html.button [ onClick "CLICK-A" ] [ Html.text "Ambiguous click" ]
                ]
            , Html.div [ id "button-b" ]
                [ Html.button [ onClick "CLICK-B" ] [ Html.text "Ambiguous click" ]
                ]
            ]
        ]


start : ProgramTest String String TestEffect
start =
    ProgramTest.createElement
        { init = \() -> testInit
        , update = testUpdate
        , view = testView
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "ProgramTest"
        [ test "has initial model" <|
            \() ->
                start
                    |> ProgramTest.expectModel (Expect.equal "<INIT>")
        , test "can send a msg" <|
            \() ->
                start
                    |> ProgramTest.update "A"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;A")
        , test "can create with flags" <|
            \() ->
                ProgramTest.createElement
                    { init = \flags -> ( "<INIT:" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    |> ProgramTest.start "flags"
                    |> ProgramTest.expectModel (Expect.equal "<INIT:flags>")
        , test "can create with JSON string flags" <|
            \() ->
                ProgramTest.createElement
                    { init = \flags -> ( "<INIT:" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view = testView
                    }
                    |> ProgramTest.withJsonStringFlags (Json.Decode.field "y" Json.Decode.string)
                    |> ProgramTest.start """{"y": "fromJson"}"""
                    |> ProgramTest.expectModel (Expect.equal "<INIT:fromJson>")
        , test "can create with navigation" <|
            \() ->
                ProgramTest.createApplication
                    { onUrlChange = .path
                    , onUrlRequest = \_ -> Debug.todo "ProgramTestTests-1:onUrlRequest"
                    , init = \() location key -> ( "<INIT:" ++ location.path ++ ">", NoOp )
                    , update = testUpdate
                    , view =
                        \model ->
                            { title = "page title"
                            , body = [ testView model ]
                            }
                    }
                    |> ProgramTest.withBaseUrl "https://example.com/path"
                    |> ProgramTest.start ()
                    |> ProgramTest.expectModel (Expect.equal "<INIT:/path>")
        , test "can create with navigation and flags" <|
            \() ->
                ProgramTest.createApplication
                    { onUrlChange = .path
                    , onUrlRequest = \_ -> Debug.todo "ProgramTestTests-4:onUrlRequest"
                    , init = \flags location key -> ( "<INIT:" ++ location.path ++ ":" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view =
                        \model ->
                            { title = "page title"
                            , body = [ testView model ]
                            }
                    }
                    |> ProgramTest.withBaseUrl "https://example.com/path"
                    |> ProgramTest.start "flags"
                    |> ProgramTest.expectModel (Expect.equal "<INIT:/path:flags>")
        , test "can assert on the view" <|
            \() ->
                start
                    |> ProgramTest.shouldHaveView
                        (Query.find [ Selector.tag "span" ] >> Query.has [ Selector.text "<INIT>" ])
                    |> ProgramTest.done
        , test "can assert on the view concisely with a terminal assertion" <|
            \() ->
                start
                    |> ProgramTest.expectView
                        (Query.find [ Selector.tag "span" ] >> Query.has [ Selector.text "<INIT>" ])
        , test "can create with navigation and JSON string flags" <|
            \() ->
                ProgramTest.createApplication
                    { onUrlChange = .path
                    , onUrlRequest = \_ -> Debug.todo "ProgramTestTests-5:onUrlRequest"
                    , init = \flags location key -> ( "<INIT:" ++ location.path ++ ":" ++ flags ++ ">", NoOp )
                    , update = testUpdate
                    , view =
                        \model ->
                            { title = "page title"
                            , body = [ testView model ]
                            }
                    }
                    |> ProgramTest.withJsonStringFlags (Json.Decode.field "x" Json.Decode.string)
                    |> ProgramTest.withBaseUrl "https://example.com/path"
                    |> ProgramTest.start """{"x": "fromJson"}"""
                    |> ProgramTest.expectModel (Expect.equal "<INIT:/path:fromJson>")
        , test "can assert on the view concisely given Html.Test.Selectors" <|
            \() ->
                start
                    |> ProgramTest.shouldHave [ Selector.tag "span" ]
                    |> ProgramTest.done
        , test "can assert on the view concisely with a terminal assertion given Html.Test.Selectors" <|
            \() ->
                start
                    |> ProgramTest.expectViewHas [ Selector.tag "span" ]
        , test "can assert on the view concisely given Html.Test.Selectors that should not exist" <|
            \() ->
                start
                    |> ProgramTest.shouldNotHave [ Selector.tag "article" ]
                    |> ProgramTest.done
        , test "can simulate an arbitrary DOM event" <|
            \() ->
                start
                    |> ProgramTest.simulate
                        (Query.find [ Selector.tag "strange" ])
                        ( "odd", Json.Encode.string "<ODD-VALUE>" )
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;<ODD-VALUE>")
        , test "can assert on the last effect after init" <|
            \() ->
                start
                    |> ProgramTest.expectLastEffect (Expect.equal NoOp)
        , test "can assert on the last effect after update" <|
            \() ->
                start
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectLastEffect (Expect.equal (LogUpdate "CLICK"))
        , test "can assert on the last effect as an intermediate assertion" <|
            \() ->
                start
                    |> ProgramTest.shouldHaveLastEffect (Expect.equal NoOp)
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.shouldHaveLastEffect (Expect.equal (LogUpdate "CLICK"))
                    |> ProgramTest.done
        , test "can simulate a response to the last effect" <|
            \() ->
                start
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.simulateLastEffect (\effect -> Ok [ Debug.toString effect ])
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;CLICK;LogUpdate \"CLICK\"")
        , test "can force a failure via simulateLastEffect" <|
            \() ->
                start
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.simulateLastEffect (\effect -> Err "Unexpected effect")
                    |> ProgramTest.done
                    |> Test.Runner.getFailureReason
                    |> Maybe.map .description
                    |> Expect.equal (Just "simulateLastEffect failed: Unexpected effect")
        , test "can be forced into failure" <|
            \() ->
                start
                    |> ProgramTest.fail "custom" "Because I said so"
                    |> ProgramTest.done
                    |> Test.Runner.getFailureReason
                    |> Maybe.map .description
                    |> Expect.equal (Just "custom: Because I said so")
        , test "can narrow down the area to specified element" <|
            \() ->
                start
                    |> ProgramTest.within
                        (Query.find [ Selector.id "button-b" ])
                        (ProgramTest.clickButton "Ambiguous click")
                    |> ProgramTest.clickButton "Click Me"
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;CLICK-B;CLICK")
        , test "can simulate setting a labeled checkbox field" <|
            \() ->
                start
                    |> ProgramTest.check "checkbox-1" "Checkbox 1" True
                    |> ProgramTest.expectModel (Expect.equal "<INIT>;Check:checkbox-1:True")
        ]
