module SimulatedEffect.Http exposing
    ( get, post
    , Body, emptyBody, stringBody, jsonBody
    , Expect, expectString, expectJson, Error
    )

{-| This module parallels [elm/http's `Http` module](https://package.elm-lang.org/packages/elm/http/2.0.0/Http).
PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using `TestContext.withSimulatedEffects`.


# Requests

@docs get, post


# Body

@docs Body, emptyBody, stringBody, jsonBody


# Expect

@docs Expect, expectString, expectJson, Error

-}

import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import SimulatedEffect as SimulatedEffect exposing (SimulatedEffect(..))


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
        , body = ""
        , onRequestComplete = onResult
        }


{-| Create a `POST` request.
-}
post :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> SimulatedEffect msg
post request =
    let
        (Expect onResult) =
            request.expect
    in
    SimulatedEffect.HttpRequest
        { method = "POST"
        , url = request.url
        , body =
            case request.body of
                EmptyBody ->
                    ""

                StringBody body ->
                    body.content
        , onRequestComplete = onResult
        }


{-| Represents the body of a `Request`.
-}
type Body
    = EmptyBody
    | StringBody
        { contentType : String
        , content : String
        }


{-| Create an empty body for your `Request`.
-}
emptyBody : Body
emptyBody =
    EmptyBody


{-| Put some JSON value in the body of your `Request`. This will automatically
add the `Content-Type: application/json` header.
-}
jsonBody : Json.Encode.Value -> Body
jsonBody value =
    StringBody
        { contentType = "application/json"
        , content = Json.Encode.encode 0 value
        }


{-| Put some string in the body of your `Request`.
-}
stringBody : String -> String -> Body
stringBody contentType content =
    StringBody
        { contentType = contentType
        , content = content
        }


{-| Logic for interpreting a response body.
-}
type Expect msg
    = Expect (Http.Response String -> msg)


{-| Expect the response body to be a `String`.
-}
expectString : (Result Http.Error String -> msg) -> Expect msg
expectString onResult =
    Expect <|
        \response ->
            case response of
                Http.BadUrl_ s ->
                    onResult (Err <| Http.BadUrl s)

                Http.Timeout_ ->
                    onResult (Err Http.Timeout)

                Http.NetworkError_ ->
                    onResult (Err Http.NetworkError)

                Http.BadStatus_ metadata body ->
                    onResult (Err <| Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    onResult (Ok body)


{-| Expect the response body to be JSON.
-}
expectJson : (Result Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson onResult decoder =
    Expect <|
        \response ->
            case response of
                Http.BadUrl_ s ->
                    onResult (Err <| Http.BadUrl s)

                Http.Timeout_ ->
                    onResult (Err Http.Timeout)

                Http.NetworkError_ ->
                    onResult (Err Http.NetworkError)

                Http.BadStatus_ metadata body ->
                    onResult (Err <| Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case Json.Decode.decodeString decoder body of
                        Err jsonError ->
                            onResult (Err <| Http.BadBody <| Json.Decode.errorToString jsonError)

                        Ok value ->
                            onResult (Ok value)


{-| -}
type alias Error =
    Http.Error
