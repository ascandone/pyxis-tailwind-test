module Main exposing (main)

import Browser
import Components.TextArea as TextArea
import Components.TextField as TextField
import Html exposing (..)
import Html.Attributes exposing (class)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    {}


type alias Model =
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    div [ class "p-8 max-w-md antialiased space-y-4" ]
        [ TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.value "Input Text"
            , TextField.placeholder "Input Text"
            ]
        , TextField.view
            [ TextField.validation (Err "Error message")
            , TextField.placeholder "Input Text"
            ]
        , TextField.view
            [ TextField.disabled True
            , TextField.placeholder "Input Text"
            ]
        , hr [] []
        , TextArea.view
            [ TextArea.value "Input Text 2"
            ]
        ]
