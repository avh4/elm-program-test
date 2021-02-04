#!/bin/bash
dir=$(cd -P -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
set -euxo pipefail

cd "$dir"
docker build . --tag elm-program-test-ci
docker run --rm elm-program-test-ci "$@"

set +x
echo "$0: SUCCESS with $(git describe --always --dirty --match "NOT A TAG")"
