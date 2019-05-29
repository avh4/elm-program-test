module SimulatedEffect.Task exposing
    ( Task, perform, attempt
    , andThen, succeed, fail
    , map
    , mapError
    )

{-| This module parallels [elm/core's `Task` module](https://package.elm-lang.org/packages/elm/core/latest/Task).
PRs are welcome to add any functions that are missing.

The functions here produce `SimulatedTasks`s instead of `Tasks`s
and `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using `TestContext.withSimulatedEffects`.


# Tasks

@docs Task, perform, attempt


# Chains

@docs andThen, succeed, fail


# Maps

@docs map


# Errors

@docs mapError

-}

import SimulatedEffect exposing (SimulatedEffect)


{-| -}
type alias Task x a =
    SimulatedEffect.SimulatedTask x a


{-| -}
perform : (a -> msg) -> Task Never a -> SimulatedEffect msg
perform f task =
    task
        |> map f
        |> mapError never
        |> SimulatedEffect.Task


{-| This is very similar to [`perform`](#perform) except it can handle failures!
-}
attempt : (Result x a -> msg) -> Task x a -> SimulatedEffect msg
attempt f task =
    task
        |> map (Ok >> f)
        |> mapError (Err >> f)
        |> SimulatedEffect.Task


{-| Chain together a task and a callback.
-}
andThen : (a -> Task x b) -> Task x a -> Task x b
andThen f task =
    case task of
        SimulatedEffect.Succeed a ->
            f a

        SimulatedEffect.Fail x ->
            SimulatedEffect.Fail x

        SimulatedEffect.HttpTask request ->
            SimulatedEffect.HttpTask
                { method = request.method
                , url = request.url
                , body = request.body
                , headers = request.headers
                , onRequestComplete = request.onRequestComplete >> andThen f
                }


{-| A task that succeeds immediately when run.
-}
succeed : a -> Task x a
succeed =
    SimulatedEffect.Succeed


{-| A task that fails immediately when run.
-}
fail : x -> Task x a
fail =
    SimulatedEffect.Fail


{-| Transform a task.
-}
map : (a -> b) -> Task x a -> Task x b
map f =
    andThen (f >> SimulatedEffect.Succeed)


{-| Transform the error value.
-}
mapError : (x -> y) -> Task x a -> Task y a
mapError f task =
    case task of
        SimulatedEffect.Succeed a ->
            SimulatedEffect.Succeed a

        SimulatedEffect.Fail x ->
            SimulatedEffect.Fail (f x)

        SimulatedEffect.HttpTask request ->
            SimulatedEffect.HttpTask
                { method = request.method
                , url = request.url
                , body = request.body
                , headers = request.headers
                , onRequestComplete = request.onRequestComplete >> mapError f
                }
