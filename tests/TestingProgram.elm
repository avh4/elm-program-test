module TestingProgram exposing (Model, Msg(..), TestContext, application, startEffects, startView, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Html exposing (Html)
import SimulatedEffect.Cmd
import TestContext exposing (SimulatedEffect)
import Url


type alias TestContext =
    TestContext.TestContext Msg Model (SimulatedEffect Msg)


startEffects : SimulatedEffect Msg -> TestContext
startEffects initialEffect =
    start initialEffect (Html.text "")


startView : Html Msg -> TestContext
startView =
    start SimulatedEffect.Cmd.none


start : SimulatedEffect Msg -> Html Msg -> TestContext
start initialEffect html =
    TestContext.createElement
        { init = \() -> ( [], initialEffect )
        , update = update
        , view = \_ -> Html.node "body" [] [ html ]
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


application : SimulatedEffect Msg -> TestContext
application initialEffects =
    TestContext.createApplication
        { onUrlChange = \location -> Log (Url.toString location)
        , onUrlRequest = \_ -> Debug.todo "TestContextTests-2:onUrlRequest"
        , init = \() location key -> ( [], initialEffects )
        , update = update
        , view =
            \_ ->
                { title = "page title"
                , body = []
                }
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.withBaseUrl "https://example.com/path"
        |> TestContext.start ()


type alias Model =
    List String


type Msg
    = Clear
    | Log String
    | ProduceEffects (SimulatedEffect Msg)


update : Msg -> Model -> ( Model, SimulatedEffect Msg )
update msg model =
    case msg of
        Clear ->
            ( []
            , SimulatedEffect.Cmd.none
            )

        Log string ->
            ( model ++ [ string ]
            , SimulatedEffect.Cmd.none
            )

        ProduceEffects effect ->
            ( model
            , effect
            )
