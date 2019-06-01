module SimulatedEffect.Cmd exposing (map)

{-| This module parallels [elm/core's `Platform.Cmd` module](https://package.elm-lang.org/packages/elm/core/1.0.2/Platform-Cmd).
PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using [`TestContext.withSimulatedEffects`](TestContext#withSimulatedEffects).

@docs map

-}

import SimulatedEffect exposing (SimulatedEffect)
import SimulatedEffect.Task as Task


{-| Transform the messages produced by a command.
-}
map : (a -> msg) -> SimulatedEffect a -> SimulatedEffect msg
map f effect =
    case effect of
        SimulatedEffect.Task t ->
            t
                |> Task.map f
                |> Task.mapError f
                |> SimulatedEffect.Task

        SimulatedEffect.PortEffect portName value ->
            SimulatedEffect.PortEffect portName value

        SimulatedEffect.PushUrl url ->
            SimulatedEffect.PushUrl url
