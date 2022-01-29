module Components.Label.Internal exposing
    ( Label
    , Position(..)
    , Size
    , Type(..)
    , medium
    , small
    , view
    )

import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Utils


type alias Label =
    { position : Position
    , label : String
    , secondaryLabel : Maybe String
    }


type Position
    = Vertical
    | Horizontal


type Type
    = Single String
    | Double String String


type Size
    = Small
    | Medium


small : Size
small =
    Small


medium : Size
medium =
    Medium


view : { size : Size, id : Maybe String, label : Label } -> Html msg
view config =
    Utils.concatArgs Html.label
        [ [ classList
                [ ( "flex flex-col gap-x-2 leading-none", True )
                , ( "justify-center items-end", config.label.position == Horizontal )
                ]
          , class <|
                case ( config.label.position, config.size ) of
                    ( Vertical, Small ) ->
                        "mb-1"

                    ( Vertical, Medium ) ->
                        "mb-2"

                    ( Horizontal, _ ) ->
                        "justify-center items-end mr-3"
          ]
        , Maybe.mapToList Html.Attributes.for config.id
        ]
        [ Html.span
            [ class "text-gray-800 font-medium"
            , class <|
                case config.size of
                    Small ->
                        "text-base"

                    Medium ->
                        "text-lg"
            ]
            [ Html.text config.label.label ]
        , Html.span [ class "text-gray-500 text-sm font-medium" ]
            [ Html.viewMaybe Html.text config.label.secondaryLabel
            ]
        ]
