module TestingProgram exposing (Model, Msg(..), TestContext, startEffects, startView, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Html exposing (Html)
import TestContext exposing (SimulatedEffect)


type alias TestContext =
    TestContext.TestContext Msg Model (List SimulatedEffect)


startEffects : List SimulatedEffect -> TestContext
startEffects initialEffects =
    start initialEffects (Html.text "")


startView : Html Msg -> TestContext
startView =
    start []


start : List SimulatedEffect -> Html Msg -> TestContext
start initialEffects html =
    TestContext.createElement
        { init = \() -> ( [], initialEffects )
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
    | ProduceEffects (List SimulatedEffect)


update : Msg -> Model -> ( Model, List SimulatedEffect )
update msg model =
    case msg of
        Clear ->
            ( [], [] )

        Log string ->
            ( model ++ [ string ]
            , []
            )

        ProduceEffects effects ->
            ( model
            , effects
            )
