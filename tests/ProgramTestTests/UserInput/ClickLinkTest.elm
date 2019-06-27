module ProgramTestTests.UserInput.ClickLinkTest exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (href)
import Html.Events
import Json.Decode
import ProgramTest exposing (ProgramTest)
import Test exposing (..)


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
            \model ->
                Html.div []
                    [ Html.a [ href "https://example.com/link" ] [ Html.text "External" ]
                    , Html.a [ href "/settings" ] [ Html.text "Relative" ]
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
        , test "can verify a relative link" <|
            \() ->
                linkProgram
                    |> ProgramTest.clickLink "Relative" "/settings"
                    |> ProgramTest.expectPageChange "http://localhost:3000/settings"
        , test "can verify an internal (single-page app) link" <|
            \() ->
                ProgramTest.createApplication
                    { onUrlChange = .path
                    , onUrlRequest = \_ -> Debug.todo "ClickLinkTest:onUrlRequest"
                    , init = \() location key -> ( "<INIT:" ++ location.path ++ ">", NoOp )
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
