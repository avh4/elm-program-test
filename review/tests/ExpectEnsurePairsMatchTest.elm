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
        , test "expect/ensure pair must have the same arguments (more than one initial argument)" <|
            \() ->
                String.join "\n"
                    [ "module ProgramTest exposing (expectSomething, ensureSomething)"
                    , "import Expect exposing (Expectation)"
                    , ""
                    , "type alias ProgramTest msg model effect = ()"
                    , ""
                    , "expectSomething : Bool -> String -> ProgramTest msg model effect -> Expectation"
                    , "expectSomething = Debug.todo \"expectSomething\""
                    , ""
                    , "ensureSomething : Bool -> Int -> ProgramTest msg model effect -> ProgramTest msg model effect"
                    , "ensureSomething = Debug.todo \"expectSomething\""
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "ensureSomething should take the same arguments as expectSomething"
                            , details =
                                [ "Assuming the type annotation for expectSomething is correct, the type annotation for ensureSomething should be:"
                                , "Bool -> String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                                ]
                            , under = "Int"
                            }
                        ]
        , test "expect/ensure pair with correct arguments" <|
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
                    , "ensureSomething : String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                    , "ensureSomething = Debug.todo \"expectSomething\""
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "ensure function must have type annotation" <|
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
                    , "ensureSomething = Debug.todo \"expectSomething\""
                    ]
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "ensureSomething must have a type annotation"
                            , details =
                                [ "Assuming the type annotation for expectSomething is correct, the type annotation for ensureSomething should be:"
                                , "String -> ProgramTest msg model effect -> ProgramTest msg model effect"
                                ]
                            , under = "ensureSomething"
                            }
                            |> Review.Test.atExactly { start = { row = 9, column = 1 }, end = { row = 9, column = 16 } }
                        ]
        ]
