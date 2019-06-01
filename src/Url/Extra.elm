module Url.Extra exposing (locationFromString, resolve)

{-| TODO: this module should implement the algorithm described at
<https://url.spec.whatwg.org/>
-}

import Url exposing (Url)


{-| Returns `Nothing` if the given string is not a valid absolute URL.
(An "absolute URL" means one starting with "<scheme>://<host>" (all other components are optional).
NOTE: the behavior of when `Nothing` is returned may change when the correct implementation from
<https://url.spec.whatwg.org/> is implemented.)
-}
locationFromString : String -> Maybe Url
locationFromString =
    Url.fromString


{-| This resolves a URL string (either an absolute or relative URL) against a base URL (given as a `Location`).
-}
resolve : Url -> String -> Url
resolve base url =
    locationFromString url
        -- TODO: implement correct logic (current logic is only correct for "authority-relative" URLs without query or fragment strings)
        |> Maybe.withDefault
            { base
                | path =
                    if String.left 1 url == "/" then
                        url

                    else
                        String.split "/" base.path
                            |> List.reverse
                            |> List.drop 1
                            |> List.reverse
                            |> (\l -> l ++ String.split "/" url)
                            |> String.join "/"
            }
