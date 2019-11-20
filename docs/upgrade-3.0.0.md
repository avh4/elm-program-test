---
id: upgrade-3.0.0
title: Upgrading from elm-program-test 2.x to 3.x
sidebar: auto
next: html.md
---

# Upgrading from elm-program-test 2.x to 3.x

The new version of elm-program-test makes some significant changes to show tests are created,
and makes a handful of other changes to the API.
The list items throughout this document starting with "👉" are a checklist of steps to follow when upgrading.


## New module name `ProgramTest`

The central module in elm-program-test has been renamed from `TestContext` to `ProgramTest`.
The new name is meant to be a more conrete and meaningful term,
and is meant to be easier to remember, as it now matches the package name (elm-program-test).
The order of the type arguments has also changed to more closely match [elm/core's `Platform.Program`](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).

- 👉 Rename all references to `TestContext` -> `ProgramTest`
- 👉 swap the order of the first two type arguments of any references to the `ProgramTest` type (`TestContext msg model effect` becomes `ProgramTest model msg effect`)


## Updated API for starting tests

In elm-program-test 2.x, there was a large set of `create...` functions
and you had to choose the one that exactly matches how you wanted to configure your test.
In **elm-program-test 3.x**, there are now three groups of functions that can be combined in different ways for more flexibility, and the new API is more aligned with the design of Elm 0.19's elm/browser package.  The new groups are as follows:

- [**`create...`** functions](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#creating-program-definitions) that parallel the functions in elm/browser that create Programs.  Use the create function that corresponds to the elm/browser function used in your Main module.  (For example, if your program uses `Browser.document`, then use `ProgramTest.createDocument` in your tests.)
- [the **`start`** function](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#start) is used to initialize the `ProgramTest` state from the `ProgramDefinition` created by the `create...` functions.  You'll pass the program's inital flags whan calling `start` (or pass `()` if your program doesn't use flags).
- [optional **`with...`** functions](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#options) that can be used to further configure your test before calling `start`.

You'll need to familiarize yourself with the `create...` and `start` functions
and update to the new API in a way that's appropriate for your specific program.

- 👉 Update creation of your tests to use the new API


## Updated API for assertions

All assertion/"should"/"expect" functions have been updated to have more consistent naming.
There are now two parallel functions for each assertion:
one starting with `expect...` which returns an `Expectation`, meaning it should be used at the end of a test;
and one starting with `ensure...` which returns a `ProgramTest`, meaning it can be used in the middle of a test.

Here's a full list of changes to existing assertion functions.

- 👉 Rename uses of the following functions:
  - `assertHttpRequest` -> `expectHttpRequestWasMade` (and it now takes the method and URL as two String parameters instead of as fields in a record parameter)
  - `shouldHave` -> `ensureViewHas`
  - `shouldNotHave` -> `ensureViewHasNot`
  - `shouldHaveView` -> `ensureView`
  - `shouldHaveLastEffect` -> `ensureLastEffect`


## Other renamed functions

- 👉 Rename uses of the following functions:
  - `simulate` -> `simulateDomEvent`


## Direct support for HTTP, ports, and time

If you are currently using `expectLastEffect` or `simulateLastEffect`
to test functionality related to HTTP requests, Elm ports, or `Task.sleep`,
there is now a much nicer API that you can use.

See the API documentation for
[`expectHttpRequestWasMade`](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#expectHttpRequestWasMade),
[`simulateHttpOk`](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#simulateHttpOk),
[`expectOutgoingPortValues`](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#expectOutgoingPortValues),
[`simulateIncomingPort`](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#simulateIncomingPort),
and [`advanceTime`](https://package.elm-lang.org/packages/avh4/elm-program-test/latest/ProgramTest#advanceTime) for more information.
There are also new guidebooks [“Testing programs with Cmds”](cmds.html)
and [“Testing programs with ports”](ports.html)
offering a more detailed example of how to set up such tests.

- 👉 Replace calls to `expectLastEffect` and `simulateLastEffect` with the nicer new API where possible
