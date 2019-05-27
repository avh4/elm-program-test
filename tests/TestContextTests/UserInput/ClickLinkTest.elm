module TestContextTests.UserInput.ClickLinkTest exposing (all)

import Expect
import Html exposing (Html)
import Html.Attributes exposing (href)
import Html.Events
import Json.Decode
import Test exposing (..)
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


linkProgram : TestContext String String TestEffect
linkProgram =
    TestContext.createWithBaseUrl
        { init = testInit
        , update = testUpdate
        , view =
            \model ->
                Html.div []
                    [ Html.a [ href "https://example.com/link" ] [ Html.text "External" ]
                    , Html.a [ href "/settings" ] [ Html.text "Relative" ]
                    ]
        }
        "http://localhost:3000/Main.elm"


all : Test
all =
    describe "TestContext.clickLinks"
        [ test "can verify an absolute link" <|
            \() ->
                linkProgram
                    |> TestContext.clickLink "External" "https://example.com/link"
                    |> TestContext.expectPageChange "https://example.com/link"
        , test "can verify a relative link" <|
            \() ->
                linkProgram
                    |> TestContext.clickLink "Relative" "/settings"
                    |> TestContext.expectPageChange "http://localhost:3000/settings"
        , test "can verify an internal (single-page app) link" <|
            \() ->
                TestContext.createWithNavigation
                    .path
                    { init = \location -> ( "<INIT:" ++ location.path ++ ">", NoOp )
                    , update = testUpdate
                    , view =
                        \_ ->
                            Html.div []
                                [ Html.a
                                    [ href "#search"
                                    , onClickPreventDefaultForLinkWithHref "GoToSearch"
                                    ]
                                    [ Html.text "SPA" ]
                                ]
                    }
                    "http://localhost:3000/"
                    |> TestContext.clickLink "SPA" "#search"
                    |> TestContext.expectModel (Expect.equal "<INIT:/>;GoToSearch")
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
