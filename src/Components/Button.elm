module Components.Button exposing
    ( Attribute
    , brand
    , ghost
    , primary
    , secondary
    , tertiary
    )

import Components.Autocomplete exposing (Event(..))
import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Utils


type alias Config msg =
    { buttonAttributes : List (Html.Attribute msg)
    }


defaultConfig : Config msg
defaultConfig =
    { buttonAttributes = []
    }


type Attribute msg
    = Attribute (Config msg -> Config msg)


makeConfig : List (Attribute msg) -> Config msg
makeConfig =
    Utils.getMakeConfig
        { unwrap = \(Attribute f) -> f
        , defaultConfig = defaultConfig
        }


type Variant
    = Primary
    | Secondary
    | Tertiary
    | Brand
    | Ghost


view : Variant -> List (Attribute msg) -> String -> Html msg
view variant attrs text_ =
    let
        config =
            makeConfig attrs
    in
    Html.button
        [ class "leading-none font-semibold outline-none"
        , classList [ ( "rounded-full min-w-[7.5rem] px-4", variant /= Ghost ) ]
        , class "text-sm h-8 "
        , class "transition-all duration-200 ease-in-out"
        , class "focus:ring ring-offset-1 ring-cyan-700/20 active:scale-[0.97]"
        , class <|
            case variant of
                Primary ->
                    "text-white bg-gradient-45-deg bg-[length:200%] bg-right hover:bg-left from-cyan-700 via-cyan-700 to-cyan-600"

                Secondary ->
                    "text-cyan-700 border-2 border-cyan-700 hover:shadow-md"

                Tertiary ->
                    "text-cyan-700 border-2 hover:border-cyan-700 hover:shadow-md"

                Brand ->
                    "text-white bg-gradient-45-deg bg-[length:200%] bg-right hover:bg-left from-fuchsia-800 via-fuchsia-800 to-fuchsia-600"

                Ghost ->
                    "rounded-lg text-cyan-700"
        ]
        [ Html.span
            [ classList [ ( "border-b-2 border-transparent hover:border-cyan-700 py-1 mx-1 transition-color duration-100 ease-in-out", variant == Ghost ) ]
            ]
            [ Html.text text_ ]
        ]


primary : List (Attribute msg) -> String -> Html msg
primary =
    view Primary


secondary : List (Attribute msg) -> String -> Html msg
secondary =
    view Secondary


tertiary : List (Attribute msg) -> String -> Html msg
tertiary =
    view Tertiary


brand : List (Attribute msg) -> String -> Html msg
brand =
    view Brand


ghost : List (Attribute msg) -> String -> Html msg
ghost =
    view Ghost
