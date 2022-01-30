module Components.Button exposing
    ( Attribute
    , brand
    , ghost
    , huge
    , large
    , loading
    , medium
    , onBlur
    , onClick
    , onFocus
    , primary
    , secondary
    , size
    , small
    , tertiary
    )

import Heroicons.Outline
import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events
import Utils


type alias Config msg =
    { buttonAttributes : List (Html.Attribute msg)
    , size : Size
    , loading : Bool
    }


defaultConfig : Config msg
defaultConfig =
    { buttonAttributes = []
    , size = Medium
    , loading = False
    }


type Attribute msg
    = Attribute (Config msg -> Config msg)


loading : Bool -> Attribute msg
loading loading_ =
    Attribute <| \c -> { c | loading = loading_ }


attribute : Html.Attribute msg -> Attribute msg
attribute attr =
    Attribute <| \c -> { c | buttonAttributes = attr :: c.buttonAttributes }


type Size
    = Small
    | Medium
    | Large
    | Huge


small : Size
small =
    Small


medium : Size
medium =
    Medium


large : Size
large =
    Large


huge : Size
huge =
    Huge


size : Size -> Attribute msg
size size_ =
    Attribute <| \c -> { c | size = size_ }


onClick : Html.Attribute msg -> Html.Attribute (Attribute msg)
onClick =
    attribute >> Html.Events.onClick


onFocus : Html.Attribute msg -> Html.Attribute (Attribute msg)
onFocus =
    attribute >> Html.Events.onClick


onBlur : Html.Attribute msg -> Html.Attribute (Attribute msg)
onBlur =
    attribute >> Html.Events.onClick



-- View


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
        [ class "leading-none text-sm font-semibold outline-none"
        , classList [ ( "rounded-2xl  px-4", variant /= Ghost ) ]
        , class <|
            case config.size of
                Small ->
                    "h-6 min-w-[5rem]"

                Medium ->
                    "h-8 min-w-[7.5rem]"

                Large ->
                    "h-10 min-w-[10rem]"

                Huge ->
                    "h-14 min-w-[13.75rem]"
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
        [ if config.loading then
            viewSpinner

          else
            Html.span
                [ classList [ ( "border-b-2 border-transparent hover:border-cyan-700 py-1 mx-1 transition-color duration-100 ease-in-out", variant == Ghost ) ]
                ]
                [ Html.text text_ ]
        ]


viewSpinner : Html msg
viewSpinner =
    Html.div [ class "flex justify-center w-full" ]
        [ Html.div
            [ class "rounded-full border-2 border-white/20 border-t-white h-6 w-6 animate-spin"
            ]
            []
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
