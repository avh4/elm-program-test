module TestProgram exposing (TestProgram)

import Test.Html.Query as Query
import Url exposing (Url)


type alias TestProgram model msg effect sub =
    { update : msg -> model -> ( model, effect )
    , view : model -> Query.Single msg
    , onRouteChange : Url -> Maybe msg
    , subscriptions : Maybe (model -> sub)
    }
