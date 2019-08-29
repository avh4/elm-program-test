[![Build Status](https://travis-ci.org/avh4/elm-program-test.svg?branch=master)](https://travis-ci.org/avh4/elm-program-test)
[![Latest Version](https://img.shields.io/elm-package/v/avh4/elm-program-test.svg?label=version)](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/)

# Test Elm Programs

`elm-program-test` provides a convenient API that works with
[elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/latest)
and [elm-html-test](http://package.elm-lang.org/packages/eeue56/elm-html-test/latest)
for testing your Elm programs as complete units.


## [Guidebooks](https://elm-program-test.netlify.com/#guidebooks)

Note: If you are just looking for a quick example of what using `elm-program-test` looks like,
see the ["basic example"](#basic-example) below.

For more detailed documentation, the following guides show examples of how to use
`elm-program-test` to test different aspects of an Elm program:

- [Testing programs with interactive views](https://elm-program-test.netlify.com/html.html) &mdash;
  shows an example of test-driving adding form validation to an Elm program
- [Testing programs with Cmds](https://elm-program-test.netlify.com/cmds.html) &mdash; shows testing a program
  that uses `Http.get` and `Http.post`
- [Testing programs with ports](https://elm-program-test.netlify.com/ports.html) &mdash; shows testing a program
  that uses ports to interface with JavaScript


## Basic example

In this example, `ProgramTest.create` is used to initiate testing of the imagined `MyProgram` module
(which follows the [Elm architecture](https://guide.elm-lang.org/architecture/)).
Then `clickButton` is used to simulate user interaction with the program,
and finally `expectViewHas` is used to assert the final state of the program's displayed HTML.

```elm
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import ProgramTest exposing (clickButton, expectViewHas, start)
import MyProgram -- just an imaginary example

exampleProgramTest : Test
exampleProgramTest =
    test "cannot publish without a title" <|
        \() ->
            ProgramTest.createElement
                { init = MyProgram.init
                , update = MyProgram.update
                , view = MyProgram.view
                }
                |> start ()
                |> clickButton "New Post"
                |> clickButton "Publish"
                |> expectViewHas
                    [ class "invalid-article"
                    , text "You must provide a title before publishing"
                    ]
```


## Testing programs with flags and/or navigation

This example tests a program that requires both [flags](https://guide.elm-lang.org/interop/javascript.html#flags) and [navigation](http://package.elm-lang.org/packages/elm-lang/navigation/latest).
There are variants of the `ProgramTest.create*` functions ([see all](ProgramTest#creating)) for all combinations of
flags and/or navigation that a program might required.

```elm
import Test exposing (..)
import Test.Html.Selector exposing (class, text)
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)
import MyProgram exposing (Flags, Msg, Model) -- just an imaginary example

start : String -> Flags -> ProgramTest Model Msg (Cmd Msg)
start initialUrl flags =
    ProgramTest.createApplication
        { onUrlChange = MyProgram.OnRouteChange
        , init = MyProgram.init -- the type of MyProgram.init is: MyProgram.Flags -> Navigation.Location -> (MyProgram.Model, Cmd MyProgram.Msg)
        , update = MyProgram.update
        , view = MyProgram.view
        }
        |> ProgramTest.withBaseUrl initialUrl
        |> ProgramTest.start flags

exampleProgramTest : Test
exampleProgramTest =
    test "pages show social media link at the end" <|
        \() ->
            start "https://my-program.example.com/page/9" MyProgram.defaultFlags
                |> clickButton "Read More"
                |> expectViewHas
                    [ class "super-social-link"
                    , attribute (href "https://super.social.example.com/avh4")
                    ]
```


## Testing view modules (not full programs)

You can also use `elm-program-test` to test things that aren't programs by creating a trivial program.

This example tests a module for a complicated view by making a program with a trivial update function:

```elm
import DateTimePicker -- using abadi199/datetimepicker 6.0.0 as an example of a view to test
import Test exposing (..)
import Test.Html.Selector exposing (text)
import ProgramTest exposing (ProgramTest, clickButton, expectViewHas)

startDatePicker :
    ProgramTest
        (DateTimePicker.State, Maybe Date) -- model: simply the state needed by the view being tested
        (DateTimePicker.State, Maybe Date) -- msg: in this trivial program, the msg is simply the new model value
        (Cmd never) -- effect: could use any type here, but Cmd seems least confusing
startDatePicker =
    ProgramTest.createElement
        { init = \() -> ((DateTimePicker.initialState, Nothing), Cmd.none)
        , update = newState model -> (newState, Cmd.none)
        , view =
            \(state, value) ->
                DateTimePicker.dateTimePicker (,) [] state value
        }
        |> ProgramTest.start ()

datePickerTest : Test
datePickerTest =
    test "can advance to the next month" <|
        \() ->
            startDatePicker
                |> clickButton "Next Month"
                |> expectViewHas [ text "April 2018" ]
```
