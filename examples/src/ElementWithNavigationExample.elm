port module ElementWithNavigationExample exposing (Effect(..), Model, Msg, init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import Url
import Url.Parser as Url exposing ((</>), int, s)


type alias Model =
    { route : Maybe Route
    }


main : Program String Model Msg
main =
    Browser.element
        { init = init >> Tuple.mapSecond perform
        , view = view
        , update = \msg -> update msg >> Tuple.mapSecond perform
        , subscriptions = subscriptions
        }


type Route
    = Blog Int


type Msg
    = UrlChanged (Maybe Route)
    | PushUrl String


type Effect
    = NoEffect
    | PushUrlEffect String


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        PushUrlEffect url ->
            pushUrl url



-- INIT


init : String -> ( Model, Effect )
init locationHref =
    ( { route = locationHrefToRoute locationHref
      }
    , NoEffect
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    onUrlChange (UrlChanged << locationHrefToRoute)



-- NAVIGATION


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


onClickPreventDefaultForLinkWithHref : msg -> Html.Attribute msg
onClickPreventDefaultForLinkWithHref msg =
    let
        isSpecialClick : Json.Decode.Decoder Bool
        isSpecialClick =
            Json.Decode.map2
                (\isCtrl isMeta -> isCtrl || isMeta)
                (Json.Decode.field "ctrlKey" Json.Decode.bool)
                (Json.Decode.field "metaKey" Json.Decode.bool)

        succeedIfFalse : a -> Bool -> Json.Decode.Decoder ( a, Bool )
        succeedIfFalse msg_ preventDefault =
            case preventDefault of
                False ->
                    Json.Decode.succeed ( msg_, True )

                True ->
                    Json.Decode.fail "succeedIfFalse: condition was True"
    in
    Html.Events.preventDefaultOn "click"
        (isSpecialClick
            |> Json.Decode.andThen (succeedIfFalse msg)
        )


link : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
link url attrs children =
    a
        (onClickPreventDefaultForLinkWithHref (PushUrl url)
            :: href url
            :: attrs
        )
        children


locationHrefToRoute : String -> Maybe Route
locationHrefToRoute locationHref =
    case Url.fromString locationHref of
        Nothing ->
            Nothing

        Just url ->
            Url.parse myParser url


myParser : Url.Parser (Route -> Route) Route
myParser =
    Url.map Blog (s "blog" </> int)



-- UPDATE


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        UrlChanged newRoute ->
            ( { model | route = newRoute }
            , NoEffect
            )

        PushUrl url ->
            ( model
            , PushUrlEffect url
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model.route of
        Nothing ->
            Html.div []
                [ link "/blog/7" [] [ Html.text "Post #7" ]
                , link "/blog/19" [] [ Html.text "Post #19" ]
                , link "/blog/23" [] [ Html.text "Post #23" ]
                ]

        Just (Blog id) ->
            Html.div []
                [ Html.text ("Blog post #" ++ String.fromInt id)
                , link "/" [] [ Html.text "Home" ]
                ]
