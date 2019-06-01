module TestingProgram exposing (Model, Msg(..), TestContext, application, start, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Html
import TestContext exposing (SimulatedEffect)
import Url


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


application : List (SimulatedEffect Msg) -> TestContext
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
        |> TestContext.withBaseUrl "https://example.com/path"
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
