module TestingProgram exposing (Model, Msg(..), ProgramTest, application, startEffects, startView, update)

{-| This is a generic program for use in tests for many elm-program-test modules.
-}

import Browser
import Html exposing (Html)
import Html.Attributes
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd
import SimulatedEffect.Navigation
import Url


type alias ProgramTest =
    ProgramTest.ProgramTest Model Msg (SimulatedEffect Msg)


startEffects : SimulatedEffect Msg -> ProgramTest
startEffects initialEffect =
    start initialEffect (Html.text "")


startView : Html Msg -> ProgramTest
startView =
    start SimulatedEffect.Cmd.none


start : SimulatedEffect Msg -> Html Msg -> ProgramTest
start initialEffect html =
    ProgramTest.createElement
        { init = \() -> ( [], initialEffect )
        , update = update
        , view = \_ -> Html.node "body" [] [ html ]
        }
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.start ()


application : SimulatedEffect Msg -> ProgramTest
application initialEffects =
    ProgramTest.createApplication
        { onUrlChange = \location -> Log ("OnUrlChange: " ++ Url.toString location)
        , onUrlRequest = UrlRequested
        , init = \() location key -> ( [], initialEffects )
        , update = update
        , view =
            \_ ->
                { title = "page title"
                , body =
                    [ Html.a [ Html.Attributes.href "/search?q=query" ] [ Html.text "SPA" ]
                    , Html.a [ Html.Attributes.href "http://external.com/test" ] [ Html.text "External" ]
                    ]
                }
        }
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.withBaseUrl "https://example.com/path"
        |> ProgramTest.start ()


type alias Model =
    List String


type Msg
    = Clear
    | Log String
    | ProduceEffects (SimulatedEffect Msg)
    | UrlRequested Browser.UrlRequest


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

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, SimulatedEffect.Navigation.pushUrl (Url.toString url) )

                Browser.External href ->
                    ( model, SimulatedEffect.Navigation.load href )
