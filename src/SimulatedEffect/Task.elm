module SimulatedEffect.Task exposing (attempt, map, mapError, perform)

import SimulatedEffect exposing (SimulatedEffect, SimulatedTask)


perform : (a -> msg) -> SimulatedTask Never a -> SimulatedEffect msg
perform f task =
    task
        |> map f
        |> mapError never
        |> SimulatedEffect.Task


attempt : (Result x a -> msg) -> SimulatedTask x a -> SimulatedEffect msg
attempt f task =
    task
        |> map (Ok >> f)
        |> mapError (Err >> f)
        |> SimulatedEffect.Task


map : (a -> b) -> SimulatedTask x a -> SimulatedTask x b
map f task =
    case task of
        SimulatedEffect.HttpRequest request ->
            SimulatedEffect.HttpRequest
                { method = request.method
                , url = request.url
                , body = request.body
                , onRequestComplete = request.onRequestComplete >> Result.map f
                }


mapError : (x -> y) -> SimulatedTask x a -> SimulatedTask y a
mapError f task =
    case task of
        SimulatedEffect.HttpRequest request ->
            SimulatedEffect.HttpRequest
                { method = request.method
                , url = request.url
                , body = request.body
                , onRequestComplete = request.onRequestComplete >> Result.mapError f
                }
