#!/bin/bash

if [ -z "$1" ]; then
	echo "$0: ERROR: git repository URL must be provided"
	exit 1
fi

if [ -z "$2" ]; then
	echo "$0: ERROR: git branch must be provided"
	exit 1
fi

set -euxo pipefail

cd /workspace
git clone --single-branch --branch "$2" "$1" elm-program-test
cd elm-program-test
git pull https://github.com/avh4/elm-program-test.git main
pnpm install

npm test

set +x
echo "$0: SUCCESS with $(cat .git/FETCH_HEAD)"
