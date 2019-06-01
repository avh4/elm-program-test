module SimulatedEffect.Port exposing (send)

{-| This module provides functions that allow you to create `SimulatedEffect`s
that parallel [Elm ports](https://guide.elm-lang.org/interop/ports.html) used in your real program.
This is meant to be used
to help you implement the function to provide when using [`TestContext.withSimulatedEffects`](TestContext#withSimulatedEffects).

@docs send

-}

import Json.Encode
import SimulatedEffect exposing (SimulatedEffect)


{-| Creates a `SimulatedEffect` that parallels using an outgoing Elm port.

For example, if your production code uses a port like this:

    port logMessage : String -> Cmd msg

    logMessage "hello"

Then the corresponding `SimulatedEffect` would be:

    SimulatedEffect.Port.send "logMessage" (Json.Encode.string "hello")

-}
send : String -> Json.Encode.Value -> SimulatedEffect msg
send =
    SimulatedEffect.PortEffect
