module VoterRegistrationExample exposing (Model, Msg(..), init, update, view)

import Browser
import Http


type alias Model =
    {}


type Msg
    = RegistrationResponse (Result Http.Error String)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body = []
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
