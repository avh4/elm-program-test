module TestingProgram exposing (Model, Msg(..), TestContext, start, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Html
import TestContext exposing (SimulatedEffect)


type alias TestContext =
    TestContext.TestContext Msg Model (List (SimulatedEffect Msg))


start : List (SimulatedEffect Msg) -> TestContext
start initialEffects =
    TestContext.createElement
        { init = \() -> ( [], initialEffects )
        , update = update
        , view = \_ -> Html.text ""
        }
        |> TestContext.withSimulatedEffects identity
        |> TestContext.start ()


type alias Model =
    List String


type Msg
    = Clear
    | Log String
    | ProduceEffects (List (SimulatedEffect Msg))


update : Msg -> Model -> ( Model, List (SimulatedEffect Msg) )
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
