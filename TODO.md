- [ ] allow navigation between different model states

Big goals:
- [ ] add tabs to see the "update focused" debugging view
- [ ] make the test result 

- As a user of elm-program-test, how do I set up my code to see test failures in the UI?


# CLI running

NOTE: also applies to running on CI

- user runs tests
- test runner compiles an Elm program that demostrates the test failure and writes it to an HTML file
    - (something) needs to know which tests failed
- user opens the HTML files
- user learns the cause of the bug!

# Interactive (watch mode)

- user starts (visual) test runner
- user finds the test they want to inspect
- user inspects the event log of the test and learns something
- user updates the test
- (visual) test runner re-runs the test


# What data is there

- the list of events that happened (user can select one)
- for a selected event:
    - the Msg that the event produced
    - the current model
        - (easy to drill down)
    - the current view
    - new subscriptions state based on the new model
    - simulation state (pending HTTP requests, Process.sleeps, etc)
    - any effects that were produced by the update call
