module VoterRegistrationExample exposing (Model, Msg(..), init, update, view)

import Browser
import Html
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode


type Model
    = Form
        { name : String
        , streetAddress : String
        , postalCode : String
        , error : Maybe String
        }
    | Loading
    | Success
        { nextElection : String
        }


type Msg
    = ChangeName String
    | ChangeStreetAddress String
    | ChangePostalCode String
    | SubmitRegistration
    | RegistrationResponse (Result Http.Error String)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( Form
        { name = ""
        , streetAddress = ""
        , postalCode = ""
        , error = Nothing
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Form info, ChangeName newName ) ->
            ( Form { info | name = newName }
            , Cmd.none
            )

        ( Form info, ChangeStreetAddress newStreetAddress ) ->
            ( Form { info | streetAddress = newStreetAddress }
            , Cmd.none
            )

        ( Form info, ChangePostalCode newPostalCode ) ->
            ( Form { info | postalCode = newPostalCode }
            , Cmd.none
            )

        ( Form info, SubmitRegistration ) ->
            let
                isValid =
                    String.length info.postalCode == 5
            in
            if isValid then
                ( Loading
                , Http.post
                    { url = "/register"
                    , body =
                        Http.jsonBody <|
                            Json.Encode.object
                                [ ( "name", Json.Encode.string info.name )
                                , ( "streetAddress", Json.Encode.string info.streetAddress )
                                , ( "postalCode", Json.Encode.string info.postalCode )
                                ]
                    , expect =
                        Http.expectJson
                            RegistrationResponse
                            (Json.Decode.field "nextElection" Json.Decode.string)
                    }
                )

            else
                ( Form { info | error = Just "You must enter a valid postal code" }
                , Cmd.none
                )

        ( Form _, _ ) ->
            ( model, Cmd.none )

        ( Loading, RegistrationResponse (Ok nextElection) ) ->
            ( Success
                { nextElection = nextElection
                }
            , Cmd.none
            )

        ( Loading, _ ) ->
            ( model, Cmd.none )

        ( Success _, _ ) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ case model of
            Form info ->
                -- TODO: should use onSubmit instead of an onClick button
                Html.form []
                    [ Html.label
                        [ for "name" ]
                        [ Html.text "Name" ]
                    , Html.input
                        [ id "name"
                        , onInput ChangeName
                        , value info.name
                        ]
                        []
                    , Html.label
                        [ for "street-address" ]
                        [ Html.text "Street Address" ]
                    , Html.input
                        [ id "street-address"
                        , onInput ChangeStreetAddress
                        , value info.streetAddress
                        ]
                        []
                    , Html.label
                        [ for "postcode" ]
                        [ Html.text "Postal Code" ]
                    , Html.input
                        [ id "postcode"
                        , onInput ChangePostalCode
                        , value info.postalCode
                        ]
                        []
                    , case info.error of
                        Nothing ->
                            Html.text ""

                        Just error ->
                            Html.text error
                    , Html.button
                        [ onClick SubmitRegistration
                        , type_ "button"
                        ]
                        [ Html.text "Register" ]
                    ]

            Loading ->
                Html.text "Loading..."

            Success info ->
                Html.div []
                    [ Html.text "Success!"
                    , Html.hr [] []
                    , Html.text ("Next election date is: " ++ info.nextElection)
                    ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
