## 3.0.0

There are many significant changes!
See [Upgrading elm-program-test 2.x to 3.x](https://elm-program-test.netlify.com/upgrade-3.0.0.html) for more details about the following changes.

API Changes:

  - The core module is renamed from `TestContext` -> `ProgramTest`
  - Redesigned API for creating and starting tests
  - Many assertion functions are renamed so that the API is more consistent
  
New features:
 
  - support for testing HTTP requests (see `expectHttpRequestWasMade` and `simulateHttpOk`)
  - support for testing ports (see `expectOutgoingPortValues` and `simulateIncomingPort`)
  - support for testing `Task.sleep` (see `advanceTime`)
  
New documentation:

  - new guidebook: [Testing programs with interactive views](https://elm-program-test.netlify.com/html.html)
  - new guidebook: [Testing programs with Cmds](https://elm-program-test.netlify.com/cmds.html)
  - new guidebook: [Testing programs with ports](https://elm-program-test.netlify.com/cmds.html)


## 2.3.2

  - dependency on `elm/json` is relaxed to `1.0.0 <= v < 2.0.0` 


## 2.3.1

New features:

  - `clickButton` now works with non-`<button>` elements having `role="button"`
  - `clickButton` now fails if the button is disabled


## 2.3.0

New features:

  - `fillIn` now works with id-less `<input>` tags that are descendants of their `<label>`
  - `fillIn` now works with `aria-label` attributes
  - added `createFailed` for use in writing helper functions that create `TestContext`s


## 2.2.0

New features:

  - functions to simulate select/option input: `selectOption`


## 2.1.0

New features:

  - added `simulateLastEffect`


## 2.0.0

Updated to support Elm 0.19.


## 1.1.0

New features:

  - functions to simulate text input: `fillIn`, `fillInTextarea`
  - functions to simulate checkbox input: `check`
  - functions to simulate and validated clicking links: `clickLink`, `expectPageChange`, `createWithBaseUrl`
  - functions for custom assertions: `within`


## 1.0.0

Initial release
