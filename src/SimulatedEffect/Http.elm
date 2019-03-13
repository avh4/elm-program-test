module SimulatedEffect.Http exposing
    ( get
    , Expect, expectString, expectJson
    )

{-| This module parallels [elm/http's `Http` module](https://package.elm-lang.org/packages/elm/http/2.0.0/Http). PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement your `deconstructEffect` function when using `TestContext.createWithSimulatedEffects`.


# Requests

@docs get


# Expect

@docs Expect, expectString, expectJson

-}

import Http
import Json.Decode exposing (Decoder)
import TestContext as SimulatedEffect exposing (SimulatedEffect(..))


{-| Create a `GET` request.
-}
get :
    { url : String
    , expect : Expect msg
    }
    -> SimulatedEffect msg
get { url, expect } =
    let
        (Expect onResult) =
            expect
    in
    SimulatedEffect.HttpRequest
        { method = "GET"
        , url = url
        , onRequestComplete = onResult
        }


{-| Logic for interpreting a response body.
-}
type Expect msg
    = Expect (Result Http.Error String -> msg)


{-| Expect the response body to be a `String`.
-}
expectString : (Result Http.Error String -> msg) -> Expect msg
expectString onResult =
    Expect onResult


{-| Expect the response body to be JSON.
-}
expectJson : (Result Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson onResult decoder =
    Expect <|
        \bodyResult ->
            case bodyResult of
                Err error ->
                    onResult (Err error)

                Ok body ->
                    case Json.Decode.decodeString decoder body of
                        Err jsonError ->
                            onResult (Err <| Http.BadBody <| Json.Decode.errorToString jsonError)

                        Ok value ->
                            onResult (Ok value)
