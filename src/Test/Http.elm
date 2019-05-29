module Test.Http exposing (hasHeader)

{-| Convenience functions for testing HTTP requests.

@docs hasHeader

-}

import Expect exposing (Expectation)
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
