module Test.Http exposing
    ( hasHeader
    , timeout, networkError, httpResponse
    )

{-| Convenience functions for testing HTTP requests.


## Expectations

@docs hasHeader


## Responses

These are ways to easily make `Http.Response` values for use with [`TestContext.simulateHttpResponse`](TestContext#simulateHttpResponse).

@docs timeout, networkError, httpResponse

-}

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Http
import SimulatedEffect


{-| Assert that the given HTTP request has the specified header.

    ...
        |> TestContext.assertHttpRequest "POST"
            "https://example.com/ok"
            (Test.Http.hasHeader "Content-Type" "application/json")
        |> ...

-}
hasHeader : String -> String -> SimulatedEffect.HttpRequest x a -> Expectation
hasHeader key value { headers } =
    let
        key_ =
            String.toLower key

        value_ =
            String.toLower value

        matches ( k, v ) =
            ( String.toLower k, String.toLower v )
                == ( key_, value_ )
    in
    if List.any matches headers then
        Expect.pass

    else
        Expect.fail <|
            String.join "\n"
                [ "Expected HTTP header " ++ key ++ ": " ++ value
                , "but got headers:"
                , List.map (\( k, v ) -> "    " ++ k ++ ": " ++ v) headers
                    |> String.join "\n"
                ]


{-| This is the same as `Http.Timeout_`,
but is exposed here so that your test doesn't need to import both `Http` and `Test.Http`.
-}
timeout : Http.Response body
timeout =
    Http.Timeout_


{-| This is the same as `Http.NetworkError_`,
but is exposed here so that your test doesn't need to import both `Http` and `Test.Http`.
-}
networkError : Http.Response body
networkError =
    Http.NetworkError_


{-| This is a more convenient way to create `Http.BadStatus_` and `Http.GoodStatus_` values.

Following the [logic in elm/http](https://github.com/elm/http/blob/2.0.0/src/Elm/Kernel/Http.js#L65),
this will produce `Http.GoodStatus_` if the given status code is in the 200 series, otherwise
it will produce `Http.BadStatus_`.

-}
httpResponse :
    { statusCode : Int
    , headers : List ( String, String )
    , body : body
    }
    -> Http.Response body
httpResponse response =
    let
        variant =
            if response.statusCode >= 200 && response.statusCode < 300 then
                Http.GoodStatus_

            else
                Http.BadStatus_
    in
    variant
        { url = ""
        , statusCode = response.statusCode
        , statusText = "TODO: if you need this, please report to https://github.com/avh4/elm-program-test/issues"
        , headers = Dict.fromList response.headers
        }
        response.body
