module Components.Label.Internal exposing
    ( Label
    , Position
    , Size
    , Type(..)
    , horizontal
    , medium
    , small
    , vertical
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


vertical : Position
vertical =
    Vertical


horizontal : Position
horizontal =
    Horizontal


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


viewLabelText : { r | size : Size, id : Maybe String } -> Label -> Html msg
viewLabelText config label_ =
    Utils.concatArgs Html.span
        [ [ classList
                [ ( "flex flex-col gap-x-2 leading-none", True )
                , ( "justify-center items-end", label_.position == Horizontal )
                ]
          , class <|
                case ( label_.position, config.size ) of
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
            [ Html.text label_.label ]
        , Html.span [ class "text-gray-500 text-sm font-medium" ]
            [ Html.viewMaybe Html.text label_.secondaryLabel
            ]
        ]


view : { size : Size, id : Maybe String, label : Maybe Label } -> Html msg -> Html msg
view args child =
    case args.label of
        Nothing ->
            child

        Just label_ ->
            Html.label
                [ class <|
                    case label_.position of
                        Vertical ->
                            "flex flex-col"

                        Horizontal ->
                            "flex"
                ]
                [ viewLabelText args label_
                , child
                ]
