module HomeAutomationExample exposing (Effect(..), Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode


apiBase =
    "http://localhost:8003/lighting_service/v1"


type WebData a
    = Loading
    | Loaded a
    | Error Http.Error


mapWebData : (a -> b) -> WebData a -> WebData b
mapWebData f data =
    case data of
        Loading ->
            Loading

        Loaded a ->
            Loaded (f a)

        Error x ->
            Error x


type alias Model =
    { lights : WebData (List Light)
    , pending : Dict String PostResult
    }


type PostResult
    = Waiting
    | Failed Http.Error


initialModel : Model
initialModel =
    { lights = Loading
    , pending = Dict.empty
    }


type Msg
    = DeviceListLoaded (Result Http.Error (List Light))
    | Set String LightState
    | LightStateChanged String (Result Http.Error Light)


type alias Light =
    { id : String
    , name : String
    , state : LightState
    }


type LightState
    = OnOff Bool
    | Dimmable Float


type alias Flags =
    ()


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


init : Flags -> ( Model, Effect )
init () =
    ( initialModel, loadDeviceList )


subscriptions _ =
    Sub.none


type Effect
    = NoEffect
    | GetDeviceList
        { url : String
        , decoder : Json.Decode.Decoder (List Light)
        , onResult : Result Http.Error (List Light) -> Msg
        }
    | ChangeLight
        { url : String
        , body : Json.Encode.Value
        , decoder : Json.Decode.Decoder Light
        , onResult : Result Http.Error Light -> Msg
        }


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        GetDeviceList { url, onResult, decoder } ->
            Http.get
                { url = url
                , expect = Http.expectJson onResult decoder
                }

        ChangeLight { url, onResult, decoder, body } ->
            Http.post
                { url = url
                , body = Http.jsonBody body
                , expect = Http.expectJson onResult decoder
                }


loadDeviceList : Effect
loadDeviceList =
    -- TODO: add auth token
    GetDeviceList
        { url = apiBase ++ "/devices"
        , decoder = Json.Decode.list lightDecoder
        , onResult = DeviceListLoaded
        }


changeLight : String -> LightState -> Effect
changeLight id newState =
    ChangeLight
        { url = apiBase ++ "/devices/" ++ id
        , body =
            Json.Encode.object
                [ ( "value"
                  , case newState of
                        OnOff True ->
                            Json.Encode.float 1.0

                        OnOff False ->
                            Json.Encode.float 0.0

                        Dimmable value ->
                            Json.Encode.float value
                  )
                ]
        , decoder = lightDecoder
        , onResult = LightStateChanged id
        }


lightDecoder : Json.Decode.Decoder Light
lightDecoder =
    let
        stateDecoder =
            Json.Decode.map2 toState
                (Json.Decode.field "dimmable" Json.Decode.bool)
                (Json.Decode.field "value" Json.Decode.float)

        toState isDimmable value =
            case isDimmable of
                True ->
                    Dimmable value

                False ->
                    if value <= 0.0 then
                        OnOff False

                    else
                        OnOff True
    in
    Json.Decode.map3 Light
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        stateDecoder


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        DeviceListLoaded (Err err) ->
            ( { model | lights = Error err }
            , NoEffect
            )

        DeviceListLoaded (Ok lights) ->
            ( { model | lights = Loaded lights }
            , NoEffect
            )

        Set id newState ->
            ( { model | pending = Dict.insert id Waiting model.pending }
            , changeLight id newState
            )

        LightStateChanged id (Err err) ->
            ( { model | pending = Dict.insert id (Failed err) model.pending }
            , NoEffect
            )

        LightStateChanged id (Ok light) ->
            let
                updateLight l =
                    if l.id == id then
                        light

                    else
                        l
            in
            ( { model
                | lights =
                    mapWebData
                        (List.map updateLight)
                        model.lights
                , pending = Dict.remove light.id model.pending
              }
            , NoEffect
            )


view model =
    { title = "Lighting control"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.lights of
        Loading ->
            Html.text "Loading..."

        Loaded lights ->
            Html.ul []
                (List.map (viewLightControl model.pending) lights)

        Error err ->
            Html.text ("Something went wrong: " ++ Debug.toString err)


viewLightControl : Dict String PostResult -> Light -> Html Msg
viewLightControl pending light =
    Html.li []
        [ viewBulb (stateToValue light.state)
        , Html.text " "
        , Html.text light.name
        , Html.text " "
        , case ( Dict.get light.id pending, light.state ) of
            ( Just Waiting, _ ) ->
                Html.text "..."

            ( Just (Failed err), _ ) ->
                Html.text ("Failed: " ++ Debug.toString err)

            ( Nothing, OnOff True ) ->
                Html.button [ onClick (Set light.id (OnOff False)) ] [ Html.text "Turn off" ]

            ( Nothing, OnOff False ) ->
                Html.button [ onClick (Set light.id (OnOff True)) ] [ Html.text "Turn on" ]

            ( Nothing, Dimmable value ) ->
                Html.span []
                    [ if value <= 0.0 then
                        Html.text "Off"

                      else if value >= 1.0 then
                        Html.text "On"

                      else
                        Html.text ("Dim (" ++ String.fromFloat value ++ ")")
                    , Html.text " "
                    , if value > 0.0 then
                        Html.button [ onClick (Set light.id (Dimmable 0.0)) ] [ Html.text "Turn off" ]

                      else
                        Html.text ""
                    , if value <= 1.0 then
                        Html.button [ onClick (Set light.id (Dimmable (value + 0.1))) ] [ Html.text "Turn up" ]

                      else
                        Html.text ""
                    , if value <= 0.5 then
                        Html.button [ onClick (Set light.id (Dimmable 1.0)) ] [ Html.text "Turn on" ]

                      else
                        Html.text ""
                    ]
        ]


stateToValue : LightState -> Float
stateToValue state =
    case state of
        OnOff True ->
            1.0

        OnOff False ->
            0.0

        Dimmable v ->
            v


viewBulb : Float -> Html msg
viewBulb value =
    Html.span
        [ style "background-color" "#333"
        , style "display" "inline-block"
        ]
        [ Html.span
            [ style "opacity" (String.fromFloat value)
            ]
            [ Html.text "💡"
            ]
        ]
