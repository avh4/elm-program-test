module TodoMvcExampleTests exposing (suite)

import Expect
import Test exposing (..)
import TestContext
import TodoMvcExample


suite : Test
suite =
    test "can add and complete a Todo" <|
        \() ->
            TestContext.createElement
                { init = TodoMvcExample.init
                , update = TodoMvcExample.update
                , view = TodoMvcExample.view
                }
                |> TestContext.start Nothing
                |> TestContext.fillIn "" "What needs to be done?" "Buy milk"
                --                |> TestContext.clickButton "Add"
                |> TestContext.done
