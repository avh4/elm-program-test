module Navigation.Extra exposing (locationFromString, resolve)

{-| TODO: this module should implement the algorithm described at
<https://url.spec.whatwg.org/>
-}

import Navigation


{-| Returns `Nothing` if the given string is not a valid absolute URL.
(An "absolute URL" means one starting with "<scheme>://<host>" (all other components are optional).
NOTE: the behavior of when `Nothing` is returned may change when the correct implentation from
<https://url.spec.whatwg.org/> is implemented.)
-}
locationFromString : String -> Maybe Navigation.Location
locationFromString url =
    if String.contains "://" url then
        Just
            { hash = "TODO"
            , host = "TODO"
            , hostname = "TODO"
            , href = url
            , origin = "TODO"
            , password = "TODO"
            , pathname = "/" ++ (url |> String.split "/" |> List.drop 3 |> String.join "/")
            , port_ = "TODO"
            , protocol = "TODO"
            , search = "TODO"
            , username = "TODO"
            }
    else
        Nothing


{-| This resolves a URL string (either an absolute or relative URL) against a base URL (given as a `Location`).
-}
resolve : Navigation.Location -> String -> Navigation.Location
resolve base url =
    locationFromString url
        -- TODO: implment correct logic (current logic is only correct for "authority-relative" URLs without query or fragment strings)
        |> Maybe.withDefault
            { base
                | href = (base.href |> String.split "/" |> List.take 3 |> String.join "/") ++ url
                , pathname = url
            }
