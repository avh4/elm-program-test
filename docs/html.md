---
id: html
title: Testing programs with interactive HTML
sidebar: auto
---

# Testing programs with interactive HTML

[`Test.Html` from the `elm-explorations/test` package](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Query)
make it possible to write unit tests for functions that return `Html msg`,
but often
(especially when your view is complicated enough to need being tested)
good test coverage requires testing the interactions between your `view` and your `update` functions
(which communicate via the `Model` and `Msg`s).

`elm-program-test` lets you write "high-level" tests for your program %%%

%%% TODO: diagram of testing boundaries 


## Introducing the example program

In this example, we'll be writing a new program from scratch,
driving the development with tests,
and using the preferred features of `elm-program-test` to
encourage writing our UI in a way that makes it follow
some accessibility best practices.

The program we'll be building is an app where people can
have spoiler-free discussions about books they are reading;
  

The program we'll be building is a solitaire (single-player) card game.

%%% TODO: sitemap of the page


## Setup

### Installing Elm and `elm-test`

Yo may prefer to follow the official installation instructions
[for Elm](https://guide.elm-lang.org/install.html)
and [for `elm-test`](https://github.com/rtfeldman/node-test-runner#readme),
but the quickest way to get started is:

```sh
mkdir card-game && cd card-game
npm install -g elm@elm0.19.0
npm install -g elm-test@elm0.19.0
```

### Initializing the project

%%%

```sh
git init
elm init
elm-test init
elm-test install avh4/elm-program-test
```




