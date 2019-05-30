module SimulatedEffect exposing (HttpRequest, SimulatedEffect(..), SimulatedTask(..))

import Http


type SimulatedEffect msg
    = Task (SimulatedTask msg msg)


type SimulatedTask x a
    = Succeed a
    | Fail x
    | HttpTask (HttpRequest x a)
    | SleepTask Float (() -> SimulatedTask x a)


type alias HttpRequest x a =
    { method : String
    , url : String
    , body : String
    , headers : List ( String, String )
    , onRequestComplete : Http.Response String -> SimulatedTask x a
    }
