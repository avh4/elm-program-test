module ExpectEnsurePairsMatchTest exposing (all)

import ExpectEnsurePairsMatch exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ExpectEnsurePairsMatch"
        [ test "expect/ensure pair must have the same arguments" <|
            \() ->
                String.join "\n"
                    [ "module ProgramTest exposing (expectSomething, ensureSomething)"
                    , "import Expect exposing (Expectation)"
                    , ""
                    , "type alias ProgramTest msg model effect = ()"
                    , ""
                    , "expectSomething : String -> ProgramTest msg model effect -> Expectation"
                    , "expectSomething = Debug.todo \"expectSomething\""
                    , ""
                    , "ensureSomething : Int -> ProgramTest msg model effect -> ProgramTest msg model effect"
                    , "ensureSomething = Debug.todo \"expectSomething\""
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "ensureSomething should take the same arguments as expectSomething"
                            , details =
                                [ "Assuming the type annotation for expectSomething is correct, the type annotation for ensureSomething should be:"
                                , "String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                                ]
                            , under = "Int"
                            }
                        ]
        ]
