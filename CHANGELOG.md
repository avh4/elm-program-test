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
