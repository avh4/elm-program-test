module ProgramTest.Program exposing (Program)

import Html exposing (Html)
import Test.Html.Query as Query
import Url exposing (Url)


{-| Since we can't inspect `Platform.Program`s in Elm,
this type represents the same thing as a record that we can access.

Note that we also parameterize `effect` and `sub` separately because
`Platform.Cmd` and `Platform.Sub` are not inspectable in Elm.

-}
type alias Program model msg effect sub =
    { update : msg -> model -> ( model, effect )
    , view : model -> Query.Single msg
    , debugView : model -> Html msg
    , onRouteChange : Url -> Maybe msg
    , subscriptions : Maybe (model -> sub)
    }
