## 4.0.0

- Upgrade to elm-explorations/test 2.0.0


## 3.8.0

- Added an unused definition to workaround issues with `elm publish`


## 3.7.0

New features:

- `clickLink` now works for internal links when testing a `Browser.application`
- `clickLink` now works for links with aria-label
- `clickLink` now works for links containing img with alt text
- `pushUrl` now works properly for relative urls with query and/or fragment
- Added `SimulatedEffect.Task.sequence`


## 3.6.3

Bug fixes:

  - Performance regression in 3.6.2 for passing tests is fixed.


## 3.6.2

New features:

  - Functions to simulate DOM events display errors more nicely:
    - Extra junk like "`✗ has text "HTML expected by the call to: ..."`" has been removed
    - All passing steps of a query up to the failure will be displayed
  - Failure messages simplify the HTML to show only what's relevant to the failure

Bug fixes:

  - Using `ensureViewHasNot` inside of `within` no longer passes when `within` fails to find its target.
  - `clickButton` works correctly with `role=button` elements when there are other `<button>` elements in view.


## 3.6.1

New features:

  - Functions to simulate DOM events now report more detailed and more relevant information in the case of failure.

Bug fixes:

  - Functions to simulate DOM events now correctly fail in all cases when there are multiple possible matches.


## 3.6.0

Changes:

  - `expectHttpRequestWasMade`, `expectHttpRequest`, and `simulateHttpResponse` will now fail if multiple pending requests have been made to the relevant (method, URL) pair.

New features:

  - Added `expectHttpRequests` for checking whether multiple requests have been made to the same endpoint (or that zero requests have been made).
  - Added `simulateHttpResponseAdvanced` for simulating HTTP responses when multiple requests have been made to the same endpoint.
  - Added `SimulatedEffect.Time.now`
  - `clickButton` can now click buttons containing only an image with alt text


## 3.5.1

Bug fixes:

  - Resolving a relative URL (such as when `clickLink` is used) is correct in more cases (specifically, when the base URL has a query or fragment string)


## 3.5.0

Packaging:

  - The `HttpRequest` type is now exposed.


## 3.4.0

New features:

  - Added to `SimulatedEffect.Task`: `map2`, `map3`, `map4`, `map5`, `onError`

Documentation:

  - Added [NavigationKeyExample](https://github.com/avh4/elm-program-test/blob/main/examples/tests/NavigationKeyExampleTest.elm) showing how to test programs that require the use of `Browser.Navigation.Key`


## 3.3.0

New features:

  - `clickButton` can now match `<button>` and `role=button` elements via their `aria-label`
  - added API for simulating browser navigation in `SimulatedEffect.Navigation`: `back`, `load`, `reload`, `reloadAndSkipCache`
  - added `ProgramTest.getOutgoingPortValues` for use in advanced helper functions


## 3.2.0

New features:

  - added `ProgramTest.expectBrowserUrl`
  - added `ProgramTest.expectBrowserHistory`
  - `clickButton` on a submit button in a form will now trigger the onSubmit of the form

Bug fixes:

  - `fillIn` will now work when the target input has both an `aria-label` and an `id`


## 3.1.0

New features:

  - added `SimulatedEffect.Http.expectStringResponse`
  - added `ProgramTest.createWorker`


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
