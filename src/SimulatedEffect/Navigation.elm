module SimulatedEffect.Navigation exposing (pushUrl, replaceUrl, back)

{-| This module parallels [elm/browsers's `Browser.Navigation` module](https://package.elm-lang.org/packages/elm/browser/1.0.1/Browser-Navigation).
_Pull requests are welcome to add any functions that are missing._

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using [`ProgramTest.withSimulatedEffects`](ProgramTest#withSimulatedEffects).


# Navigate within Page

@docs pushUrl, replaceUrl, back

-}

import SimulatedEffect exposing (SimulatedEffect)


{-| Change the URL, but do not trigger a page load.
This will add a new entry to the browser history.
-}
pushUrl : String -> SimulatedEffect msg
pushUrl =
    SimulatedEffect.PushUrl


{-| Change the URL, but do not trigger a page load.
This _will not_ add a new entry to the browser history.
-}
replaceUrl : String -> SimulatedEffect msg
replaceUrl =
    SimulatedEffect.ReplaceUrl


{-| Go back some number of pages.
-}
back : Int -> SimulatedEffect msg
back =
    SimulatedEffect.Back
