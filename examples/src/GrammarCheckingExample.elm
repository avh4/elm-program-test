port module GrammarCheckingExample exposing (Effect(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onClick, onInput)


port checkGrammar : String -> Cmd msg


port grammarCheckResults : (List String -> msg) -> Sub msg


type alias Flags =
    ()


type alias Model =
    { text : String
    , results : List String
    }


initialModel : Model
initialModel =
    { text = ""
    , results = []
    }


type Msg
    = ChangeText String
    | CheckGrammar
    | GrammarCheckResults (List String)


main : Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , subscriptions = subscriptions
        , view = view
        }


type Effect
    = NoEffect
    | CheckGrammarEffect String


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        CheckGrammarEffect text ->
            checkGrammar text


init : Flags -> ( Model, Effect )
init () =
    ( initialModel
    , NoEffect
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    grammarCheckResults GrammarCheckResults


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        ChangeText newText ->
            ( { model | text = newText }
            , NoEffect
            )

        CheckGrammar ->
            ( model
            , CheckGrammarEffect model.text
            )

        GrammarCheckResults results ->
            ( { model | results = results }
            , NoEffect
            )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Html.label
            [ for "main" ]
            [ Html.text "Enter text to check" ]
        , Html.input
            [ id "main"
            , onInput ChangeText
            ]
            []
        , Html.ul []
            (List.map (\result -> Html.li [] [ Html.text result ]) model.results)
        , Html.button [ onClick CheckGrammar ] [ Html.text "Check" ]
        ]
    }
