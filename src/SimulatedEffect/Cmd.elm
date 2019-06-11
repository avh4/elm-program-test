module SimulatedEffect.Cmd exposing (none)

{-| This module parallels [elm/core's `Platform.Cmd` module](https://package.elm-lang.org/packages/elm/core/1.0.2/Platform-Cmd).
PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using [`TestContext.withSimulatedEffects`](TestContext#withSimulatedEffects).

@docs none

-}

import SimulatedEffect exposing (SimulatedEffect)


{-| Tell the runtime that there are no commands.
-}
none : SimulatedEffect msg
none =
    SimulatedEffect.None
