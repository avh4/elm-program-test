#!/bin/bash

# To generate the code for an ellie-app.com example,
# choose only one of the EXAMPLE= lines below,
# run the script from the project root
# and paste the output into a new ellie-app document.
# Then import the necessary packages until it runs,
# set the title for the example,
# and save it,
# and update the link in the corresponding docs/*.md file.

EXAMPLE="VoterRegistrationExample"
EXAMPLE="HomeAutomationExample"
EXAMPLE="GrammarCheckingExample"

# module line
if grep -q '^port ' examples/src/"$EXAMPLE".elm; then
    echo -n "port "
fi
echo "module Main exposing (main)"
echo

# import lines
cat examples/src/"$EXAMPLE".elm | sed -n -e '/^import /p'
cat examples/tests/"$EXAMPLE"Test.elm | sed -n -e "/import $EXAMPLE/d;/^import /p"
echo "import Test.Runner.Html"
echo "import Random"

# definitions
cat examples/src/"$EXAMPLE".elm | sed -e '/^\(port \)*module /d;/^import /d;s/^main /main_ /'
cat examples/tests/"$EXAMPLE"Test.elm | sed -e '/^module /d;/^import /d;s/Main\.//g;s/|> update/|> ProgramTest.update/'

# test runner
cat <<EOF


main =
    let
        config =
            Test.Runner.Html.defaultConfig (Random.initialSeed 0)
    in
    Test.Runner.Html.viewResults config all
EOF
