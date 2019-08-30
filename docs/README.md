---
id: intro
title: Guide to elm-program-test
---

# Testing Elm Programs

`elm-program-test` provides a convenient API that works with
[elm-test](http://package.elm-lang.org/packages/elm-explorations/test/latest)
(including `Test.Html`)
for testing your Elm programs as complete units.
Testing your programs at this level
provides test coverage that is resilient even to drastic refactorings of your application architecture,
and encourages tests that make it clear how end-users and external services will interact with your program.

- [API documentation](https://package.elm-lang.org/packages/avh4/elm-program-test/latest)
- [GitHub page](https://github.com/avh4/elm-program-test)


## Guidebooks

The following guides show examples of how to use
`elm-program-test` to test different aspects of an Elm program.
You can skip to whichever guide is relevant to what you want to
test in your program.

- [Testing programs with interactive views](html.md) &mdash;
  shows an example of test-driving adding form validation to an Elm program
- [Testing programs with Cmds](cmds.md) &mdash; shows testing a program
  that uses `Http.get` and `Http.post`
- [Testing programs with ports](ports.md) &mdash; shows testing a program
  that uses ports to interface with JavaScript
- [Upgrading from elm-program-test 2.x to 3.x](upgrade-3.0.0.md)

