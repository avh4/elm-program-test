module TestingProgram exposing (Model, Msg(..), TestContext, startEffects, startView, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Html exposing (Html)
import SimulatedEffect.Cmd
import TestContext exposing (SimulatedEffect)


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
