module ProgramTestTests.UserInput.ClickLinkTest exposing (all)

import Expect
import Html
import Html.Attributes exposing (href)
import Html.Events
import Json.Decode
import ProgramTest exposing (ProgramTest)
import SimulatedEffect
import Test exposing (..)
import Test.Expect exposing (expectFailure)
import TestingProgram exposing (Msg(..))


type TestEffect
    = NoOp
    | LogUpdate String


testUpdate : String -> String -> ( String, TestEffect )
testUpdate msg model =
    ( model ++ ";" ++ msg
    , LogUpdate msg
    )


linkProgram : ProgramTest String String ()
linkProgram =
    ProgramTest.createSandbox
        { init = "<INIT>"
        , update = \msg model -> model ++ ";" ++ msg
        , view =
            \_ ->
                Html.div []
                    [ Html.a [ href "https://example.com/link" ] [ Html.text "External" ]
                    , Html.a [ href "/settings" ] [ Html.text "Relative" ]
                    , Html.a
                        [ href "https://example.com/link"
                        , Html.Attributes.attribute "aria-label" "Aria"
                        ]
                        []
                    , Html.a [ href "https://example.com/link" ] [ Html.img [ Html.Attributes.alt "Alt Text" ] [] ]
                    ]
        }
        |> ProgramTest.withBaseUrl "http://localhost:3000/Main.elm"
        |> ProgramTest.start ()


all : Test
all =
    describe "ProgramTest.clickLinks"
        [ test "can verify an absolute link" <|
            \() ->
                linkProgram
                    |> ProgramTest.clickLink "External" "https://example.com/link"
                    |> ProgramTest.expectPageChange "https://example.com/link"
        , test "can verify a link with aria-label" <|
            \() ->
                linkProgram
                    |> ProgramTest.clickLink "Aria" "https://example.com/link"
                    |> ProgramTest.expectPageChange "https://example.com/link"
        , test "can verify a link with img and alt text" <|
            \() ->
                linkProgram
                    |> ProgramTest.clickLink "Alt Text" "https://example.com/link"
                    |> ProgramTest.expectPageChange "https://example.com/link"
        , test "can verify a relative link" <|
            \() ->
                linkProgram
                    |> ProgramTest.clickLink "Relative" "/settings"
                    |> ProgramTest.expectPageChange "http://localhost:3000/settings"
        , test "can verify an internal (single-page app) link with onClick handler" <|
            \() ->
                ProgramTest.createApplication
                    { onUrlChange = .path
                    , onUrlRequest = \_ -> Debug.todo "ClickLinkTest:onUrlRequest"
                    , init = \() location () -> ( "<INIT:" ++ location.path ++ ">", NoOp )
                    , update = testUpdate
                    , view =
                        \_ ->
                            { title = "page title"
                            , body =
                                [ Html.a
                                    [ href "#search"
                                    , onClickPreventDefaultForLinkWithHref "GoToSearch"
                                    ]
                                    [ Html.text "SPA" ]
                                ]
                            }
                    }
                    |> ProgramTest.withBaseUrl "http://localhost:3000/"
                    |> ProgramTest.start ()
                    |> ProgramTest.clickLink "SPA" "#search"
                    |> ProgramTest.expectModel (Expect.equal "<INIT:/>;GoToSearch")
        , test "internal (single-page app) link causes url change" <|
            \() ->
                TestingProgram.application SimulatedEffect.None
                    |> ProgramTest.clickLink "SPA" "/search?q=query"
                    |> ProgramTest.ensureBrowserHistory (Expect.equal [ "https://example.com/path" ])
                    |> ProgramTest.ensureBrowserUrl (Expect.equal "https://example.com/search?q=query")
                    |> ProgramTest.expectModel (Expect.equal [ "OnUrlChange: https://example.com/search?q=query" ])
        , test "external link changes page" <|
            \() ->
                TestingProgram.application SimulatedEffect.None
                    |> ProgramTest.clickLink "External" "http://external.com/test"
                    |> ProgramTest.expectPageChange "http://external.com/test"
        , test "gives accessibility advice for links with bad onClick" <|
            \() ->
                TestingProgram.startView
                    (Html.a
                        [ href "#search"
                        , Html.Events.onClick (Log "Click")
                        ]
                        [ Html.text "SPA" ]
                    )
                    |> ProgramTest.clickLink "SPA" "#search"
                    |> ProgramTest.done
                    |> expectFailure
                        [ """clickLink "SPA": Found an `<a href="...">` tag has an onClick handler, but the handler is overriding ctrl-click and meta-click."""
                        , ""
                        , "A properly behaved single-page app should not override ctrl- and meta-clicks on `<a>` tags because this prevents users from opening links in new tabs/windows."
                        , ""
                        , "Use `onClickPreventDefaultForLinkWithHref` defined at <https://gist.github.com/avh4/712d43d649b7624fab59285a70610707> instead of `onClick` to fix this problem."
                        , ""
                        , "See discussion of this issue at <https://github.com/elm-lang/navigation/issues/13>."
                        ]
        ]


onClickPreventDefaultForLinkWithHref : msg -> Html.Attribute msg
onClickPreventDefaultForLinkWithHref msg =
    let
        isSpecialClick : Json.Decode.Decoder Bool
        isSpecialClick =
            Json.Decode.map2
                (\isCtrl isMeta -> isCtrl || isMeta)
                (Json.Decode.field "ctrlKey" Json.Decode.bool)
                (Json.Decode.field "metaKey" Json.Decode.bool)
    in
    Html.Events.preventDefaultOn "click"
        (isSpecialClick
            |> Json.Decode.andThen (succeedIfFalse msg)
            |> Json.Decode.map (\m -> ( m, True ))
        )


succeedIfFalse : a -> Bool -> Json.Decode.Decoder a
succeedIfFalse msg preventDefault =
    case preventDefault of
        False ->
            Json.Decode.succeed msg

        True ->
            Json.Decode.fail "succeedIfFalse: condition was True"
