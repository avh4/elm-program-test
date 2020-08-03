module NavigationKeyExample exposing
    ( main
    , Model, Msg(..), Effect(..), init, update, view
    )

{-|

@docs main


# Exposed for tests

@docs Model, Msg, Effect, init, update, view

-}

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes
import Html.Events exposing (onClick)
import Process
import Task
import Url exposing (Url)
import Url.Parser exposing ((</>))


type alias Flags =
    ()


type Route
    = Home
    | User Int
    | Page String
    | NotFound


parseRoute : Url -> Route
parseRoute url =
    let
        parser =
            Url.Parser.oneOf
                [ Url.Parser.map Home Url.Parser.top
                , Url.Parser.map User (Url.Parser.s "users" </> Url.Parser.int)
                , Url.Parser.map Page Url.Parser.string
                ]
    in
    Url.Parser.parse parser url
        |> Maybe.withDefault NotFound


type alias Model navigationKey =
    { route : Route
    , navigationKey : navigationKey
    }


main : Program Flags (Model Navigation.Key) Msg
main =
    let
        performEffect ( model, effect ) =
            ( model, perform model.navigationKey effect )
    in
    Browser.application
        { init = \flags url key -> init flags url key |> performEffect
        , view = view
        , update = \msg model -> update msg model |> performEffect
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : flags -> Url -> navigationKey -> ( Model navigationKey, Effect )
init _ url navigationKey =
    ( { route = parseRoute url
      , navigationKey = navigationKey
      }
    , NoEffect
    )


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | GoHomeIn3
    | GoHomeNow


update : Msg -> Model navigationKey -> ( Model navigationKey, Effect )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, PushUrl (Url.toString url) )

        OnUrlRequest (Browser.External string) ->
            ( model, Load string )

        OnUrlChange url ->
            ( { model | route = parseRoute url }
            , NoEffect
            )

        GoHomeIn3 ->
            ( model, Delay 3000 GoHomeNow )

        GoHomeNow ->
            ( model, PushUrl "/" )


type Effect
    = NoEffect
    | Delay Float Msg
    | PushUrl String
    | Load String


perform : Navigation.Key -> Effect -> Cmd Msg
perform navigationKey effect =
    case effect of
        NoEffect ->
            Cmd.none

        Delay ms msg ->
            Process.sleep ms
                |> Task.perform (\() -> msg)

        PushUrl string ->
            Navigation.pushUrl navigationKey string

        Load string ->
            Navigation.load string


view : Model navigationKey -> Browser.Document Msg
view model =
    { title = "elm-program-test: NavigationKey example"
    , body =
        [ Html.header []
            [ Html.a [ Html.Attributes.href "/users/2" ] [ Html.text "User 2" ]
            ]
        , Html.main_ []
            [ Html.h1 []
                [ Html.text (Debug.toString model.route)
                ]
            ]
        , Html.footer []
            [ Html.button [ onClick GoHomeIn3 ] [ Html.text "Go Home in 3 seconds" ]
            ]
        ]
    }
