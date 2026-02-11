module ProgramTestTests.GetModelTest exposing (all)

import Expect
import Html
import Html.Events exposing (onClick)
import ProgramTest exposing (ProgramTest)
import Test exposing (..)


type alias Model =
    { count : Int
    , label : String
    }


type Msg
    = Increment
    | SetLabel String


start : ProgramTest Model Msg ()
start =
    ProgramTest.createSandbox
        { init = { count = 0, label = "initial" }
        , update =
            \msg model ->
                case msg of
                    Increment ->
                        { model | count = model.count + 1 }

                    SetLabel s ->
                        { model | label = s }
        , view =
            \model ->
                Html.div []
                    [ Html.span [] [ Html.text (String.fromInt model.count) ]
                    , Html.button [ onClick Increment ] [ Html.text "+" ]
                    ]
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "getModel"
        [ test "returns the initial model" <|
            \() ->
                start
                    |> ProgramTest.getModel
                    |> Expect.equal (Ok { count = 0, label = "initial" })
        , test "returns the model after interactions" <|
            \() ->
                start
                    |> ProgramTest.clickButton "+"
                    |> ProgramTest.clickButton "+"
                    |> ProgramTest.clickButton "+"
                    |> ProgramTest.getModel
                    |> Result.map .count
                    |> Expect.equal (Ok 3)
        , test "returns Err when the ProgramTest is in a failed state" <|
            \() ->
                start
                    |> ProgramTest.clickButton "nonexistent button"
                    |> ProgramTest.getModel
                    |> isErr
                    |> Expect.equal True
        , test "error message describes the original failure" <|
            \() ->
                start
                    |> ProgramTest.clickButton "nonexistent button"
                    |> ProgramTest.getModel
                    |> Result.mapError (String.contains "nonexistent button")
                    |> Expect.equal (Err True)
        , test "returns Err for a program that failed to create" <|
            \() ->
                ProgramTest.createFailed "setup" "bad config"
                    |> ProgramTest.getModel
                    |> isErr
                    |> Expect.equal True
        , test "returns the model after update" <|
            \() ->
                start
                    |> ProgramTest.update (SetLabel "updated")
                    |> ProgramTest.getModel
                    |> Result.map .label
                    |> Expect.equal (Ok "updated")
        , test "returns the model from a worker program" <|
            \() ->
                ProgramTest.createWorker
                    { init = \() -> ( "worker-init", () )
                    , update = \msg model -> ( model ++ ";" ++ msg, () )
                    }
                    |> ProgramTest.start ()
                    |> ProgramTest.update "hello"
                    |> ProgramTest.getModel
                    |> Expect.equal (Ok "worker-init;hello")
        ]


isErr : Result a b -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False
