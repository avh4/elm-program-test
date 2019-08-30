module SimulatedEffect.Task exposing
    ( perform, attempt
    , andThen, succeed, fail
    , map
    , mapError
    )

{-| This module parallels [elm/core's `Task` module](https://package.elm-lang.org/packages/elm/core/1.0.2/Task).
_Pull requests are welcome to add any functions that are missing._

The functions here produce `SimulatedTasks`s instead of `Tasks`s
and `SimulatedEffect`s instead of `Cmd`s, which are meant to be used
to help you implement the function to provide when using [`ProgramTest.withSimulatedEffects`](ProgramTest#withSimulatedEffects).


# Tasks

@docs perform, attempt


# Chains

@docs andThen, succeed, fail


# Maps

@docs map


# Errors

@docs mapError

-}

import SimulatedEffect exposing (SimulatedEffect, SimulatedTask)


{-| -}
perform : (a -> msg) -> SimulatedTask Never a -> SimulatedEffect msg
perform f task =
    task
        |> map f
        |> mapError never
        |> SimulatedEffect.Task


{-| This is very similar to [`perform`](#perform) except it can handle failures!
-}
attempt : (Result x a -> msg) -> SimulatedTask x a -> SimulatedEffect msg
attempt f task =
    task
        |> map (Ok >> f)
        |> mapError (Err >> f)
        |> SimulatedEffect.Task


{-| Chain together a task and a callback.
-}
andThen : (a -> SimulatedTask x b) -> SimulatedTask x a -> SimulatedTask x b
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

        SimulatedEffect.SleepTask delay onResult ->
            SimulatedEffect.SleepTask delay (onResult >> andThen f)


{-| A task that succeeds immediately when run.
-}
succeed : a -> SimulatedTask x a
succeed =
    SimulatedEffect.Succeed


{-| A task that fails immediately when run.
-}
fail : x -> SimulatedTask x a
fail =
    SimulatedEffect.Fail


{-| Transform a task.
-}
map : (a -> b) -> SimulatedTask x a -> SimulatedTask x b
map f =
    andThen (f >> SimulatedEffect.Succeed)


{-| Transform the error value.
-}
mapError : (x -> y) -> SimulatedTask x a -> SimulatedTask y a
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

        SimulatedEffect.SleepTask delay onResult ->
            SimulatedEffect.SleepTask delay (onResult >> mapError f)
