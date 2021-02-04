
# Development environment

### Setup

```sh
npm install
(cd examples && npm install)
```

### Running test

This will run everything that CI runs:

```sh
npm test
```

### Watch mode and local dev server(s):

```sh
npm start
```

This will start the following:

- Re-run `npm test` whenever a `*.elm` file changes (output is prefixed with `[watch]`)
- On <http://localhost:8001/>: Preview of the package documentation
- On <http://localhost:8002/src/>: The elm-reactor dev server (for `examples/`)
- On <http://localhost:8003/>: The backend for the examples (you won't need to look at this directly)
- On <http://localhost:8004/>: Preview of the elm-program-test guide (for `docs/`)

(If you need to change any of the ports use by the local services, edit `package.json`
-- but remember not to commit those changes!)


# CI

To do a CI run:

- you will need [docker](https://docs.docker.com/get-docker/)
- push your branch to a publicly-accessible git repo
- Then run:

```sh
./ci/run-in-docker.sh <git repo URL> <git branch>
```
