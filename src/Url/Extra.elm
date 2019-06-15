module Url.Extra exposing (locationFromString, resolve)

{-| TODO: this module should implement the algorithm described at
<https://url.spec.whatwg.org/>
-}

import Url exposing (Url)


{-| Returns `Nothing` if the given string is not a valid absolute URL.
(An "absolute URL" means one starting with "<scheme>://<host>" (all other components are optional).
NOTE: the behavior when `Nothing` is returned may change when the correct implementation from
<https://url.spec.whatwg.org/> is implemented.)
-}
locationFromString : String -> Maybe Url
locationFromString url =
    if String.contains "://" url then
        Just
            { protocol =
                url
                    |> String.split "://"
                    |> List.head
                    |> Maybe.andThen stringToProtocol
                    -- NOTE: this is technically incorrect because Url does not currently allow representation of URLs with protocols that aren't HTTP or HTTPS
                    |> Maybe.withDefault Url.Https
            , host = url |> String.split "/" |> List.drop 2 |> List.head |> Maybe.withDefault ""
            , port_ = Nothing -- TODO
            , path = "/" ++ (url |> String.split "/" |> List.drop 3 |> String.join "/")
            , query = Nothing -- TODO
            , fragment = Nothing -- TODO
            }

    else
        Nothing


stringToProtocol : String -> Maybe Url.Protocol
stringToProtocol string =
    case string of
        "http" ->
            Just Url.Http

        "https" ->
            Just Url.Https

        _ ->
            Nothing


{-| This resolves a URL string (either an absolute or relative URL) against a base URL (given as a `Location`).
-}
resolve : Url -> String -> Url
resolve base url =
    locationFromString url
        -- TODO: implement correct logic (current logic is only correct for "authority-relative" URLs without query or fragment strings)
        |> Maybe.withDefault
            { base
                | path = url
            }
