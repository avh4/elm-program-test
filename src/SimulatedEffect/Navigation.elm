module SimulatedEffect.Navigation exposing (pushUrl)

{-| This module parallels [elm/browsers's `Browser.Navigation` module](https://package.elm-lang.org/packages/elm/browser/1.0.1/Browser-Navigation).
PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using [`TestContext.withSimulatedEffects`](TestContext#withSimulatedEffects).


# Navigate within Page

@docs pushUrl

-}

import SimulatedEffect exposing (SimulatedEffect)


{-| Change the URL, but do not trigger a page load.
-}
pushUrl : String -> SimulatedEffect msg
pushUrl =
    SimulatedEffect.PushUrl
