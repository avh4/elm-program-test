module NavigationKeyExample exposing
    ( main
    , Model, Msg(..), init, update, view
    )

{-|

@docs main


# Exposed for tests

@docs Model, Msg, init, update, view

-}

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser exposing ((</>))


type alias Flags =
    ()


type Route
    = Home
    | User Int
    | Page String
    | NotFound


parseRoute : Url -> Maybe Route
parseRoute =
    let
        parser =
            Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map User (Url.Parser.s "users" </> Url.Parser.int)
                , Url.Parser.map Page Url.Parser.string
                ]
    in
    Url.Parser.parse parser


type alias Model navigationKey =
    { route : Route
    , navigationKey : navigationKey
    }


main : Program Flags (Model Navigation.Key) Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : flags -> Url -> navigationKey -> ( Model navigationKey, Cmd msg )
init _ url navigationKey =
    ( { route =
            parseRoute url
                |> Maybe.withDefault NotFound
      , navigationKey = navigationKey
      }
    , Cmd.none
    )


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


update : Msg -> model -> ( model, Cmd msg )
update msg model =
    case msg of
        OnUrlRequest urlRequest ->
            ( model, Cmd.none )

        OnUrlChange url ->
            ( model, Cmd.none )


view : Model navigationKey -> Browser.Document msg
view model =
    { title = "elm-program-test: NavigationKey example"
    , body =
        [ Html.header []
            [ Html.a [ Html.Attributes.href "/users/2" ] [ Html.text "User 2" ]
            ]
        , Html.h1 []
            [ Html.text (Debug.toString model.route)
            ]
        ]
    }
