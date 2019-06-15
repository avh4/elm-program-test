module SimulatedEffect exposing (HttpRequest, SimulatedEffect(..), SimulatedTask(..))

{-| This module contains the simulated effects that you can use with `TestContext.withSimulatedEffects`.

This module contains the lowest-level effects you can simulate.
For a higher-level API that mimics the the real-world APIs, see:

  - `SimulatedEffect.Http`

-}

import Http


{-| This represents an effect that elm-program-test is able to simulate.
When using `withSimulatedEffects` you will provide a function that can translate
your programs' effects into `SimulatedEffect`s.
(If you do not use `withSimulatedEffects`,
then `TestContext` will not simulate any HTTP effects for you.)
-}
type SimulatedEffect msg
    = None
    | Batch (List (SimulatedEffect msg))
    | Task (SimulatedTask msg msg)


type SimulatedTask x a
    = Succeed a
    | Fail x
    | HttpTask (HttpRequest x a)


type alias HttpRequest x a =
    { method : String
    , url : String
    , body : String
    , headers : List ( String, String )
    , onRequestComplete : Http.Response String -> SimulatedTask x a
    }
