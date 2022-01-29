module Main exposing (main)

import Browser
import Components.TextArea as TextArea
import Components.TextField as TextField
import FeatherIcons
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


header : String -> Html msg
header text_ =
    Html.h3 [ Html.Attributes.class "pt-3 text-3xl tracking-wide text-gray-900 font-serif" ] [ Html.text text_ ]


view : Model -> Html Msg
view _ =
    div [ class "p-8 max-w-md antialiased space-y-6" ]
        [ header "Default"
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
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.size TextField.small
            ]
        , header "Labeled"
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.vertical (TextField.single "Label")
            , TextField.id "item-id"
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.vertical (TextField.double "Label" "Second label")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.horizontal (TextField.single "Label")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.horizontal (TextField.double "Label" "Second label")
            ]

        -- Small
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.vertical (TextField.single "Label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.vertical (TextField.double "Label" "Second label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.horizontal (TextField.single "Label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label TextField.horizontal (TextField.double "Label" "Second label")
            , TextField.size TextField.small
            ]
        , header "Text addon"
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.trailing (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.validation (Err "Error message")
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.disabled True
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.size TextField.small
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , header "Icon addon"
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.trailing (TextField.iconAddon FeatherIcons.link)
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.validation (Err "Error message")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.disabled True
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.size TextField.small
            ]
        , header "Textarea"
        , TextArea.view
            [ TextArea.value "Input Text 2"
            ]
        ]
