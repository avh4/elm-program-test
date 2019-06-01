module SimulatedEffect exposing (HttpRequest, SimulatedEffect(..), SimulatedSub(..), SimulatedTask(..))

import Http
import Json.Decode
import Json.Encode


type SimulatedEffect msg
    = Task (SimulatedTask msg msg)
    | PortEffect String Json.Encode.Value
    | PushUrl String
    | ReplaceUrl String


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


type SimulatedSub msg
    = PortSub String (Json.Decode.Decoder msg)
