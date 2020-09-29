module VoterRegistrationExampleTest exposing (all, successfulRegistration)

import ProgramTest exposing (ProgramTest, clickButton, done, ensureViewHas, expectViewHas, fillIn, update)
import Test exposing (..)
import Test.Html.Selector exposing (text)
import VoterRegistrationExample as Main


start : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
start =
    ProgramTest.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start ()


successfulRegistration : ProgramTest Main.Model Main.Msg (Cmd Main.Msg)
successfulRegistration =
    start
        |> fillIn "name" "Name" "Bailey Sheppard"
        |> fillIn "street-address" "Street Address" "14 North Moore Street"
        |> fillIn "postcode" "Postal Code" "60777606"
        |> clickButton "Register"
        |> update (Main.RegistrationResponse (Ok "Aug 12"))
        |> ensureViewHas
            [ text "Success!"
            , text "Next election date is: Aug 12"
            ]


all : Test
all =
    describe "voter registration frontend"
        [ test "happy path: successful registration" <|
            \() ->
                done successfulRegistration
        , test "invalid postal code shows a validation error" <|
            \() ->
                start
                    |> fillIn "name" "Name" "Bailey Sheppard"
                    |> fillIn "street-address" "Street Address" "14 North Moore Street"
                    |> fillIn "postcode" "Postal Code" "0000"
                    |> clickButton "Register"
                    |> expectViewHas
                        [ text "You must enter a valid postal code"
                        ]
        ]
