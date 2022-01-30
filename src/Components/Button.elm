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


minWidthClass : Size -> Html.Attribute msg
minWidthClass size_ =
    class <|
        case size_ of
            Small ->
                "min-w-[5rem]"

            Medium ->
                "min-w-[7.5rem]"

            Large ->
                "min-w-[10rem]"

            Huge ->
                "min-w-[13.75rem]"


sizeClass : Size -> Html.Attribute msg
sizeClass size_ =
    class <|
        case size_ of
            Small ->
                "h-6"

            Medium ->
                "h-8"

            Large ->
                "h-10"

            Huge ->
                "h-14"


view : Variant -> List (Attribute msg) -> String -> Html msg
view variant attrs text_ =
    let
        config =
            makeConfig attrs
    in
    Utils.concatArgs Html.button
        [ [ class "leading-none text-sm font-semibold outline-none"
          , class "transition-all duration-200 ease-in-out"
          , class "focus:ring ring-offset-1 ring-cyan-700/20 active:scale-[0.97]"
          , sizeClass config.size
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
        , case variant of
            Ghost ->
                []

            _ ->
                [ class "rounded-2xl  px-4"
                , minWidthClass config.size
                ]
        , config.buttonAttributes
        ]
        [ if config.loading && variant /= Ghost then
            viewSpinner variant config

          else
            viewTextContent
                { isGhost = variant == Ghost
                , text_ = text_
                }
        ]


isBackgroundFilled : Variant -> Bool
isBackgroundFilled variant =
    case variant of
        Primary ->
            True

        Secondary ->
            False

        Tertiary ->
            False

        Brand ->
            True

        Ghost ->
            False


viewSpinner : Variant -> { r | size : Size } -> Html msg
viewSpinner variant config =
    Html.div [ class "flex justify-center w-full" ]
        [ Html.div
            [ class "rounded-full border-2 animate-spin"
            , class <|
                if isBackgroundFilled variant then
                    "border-white/20 border-t-white"

                else
                    "border-cyan-800/20 border-t-cyan-800"
            , class <|
                case config.size of
                    Small ->
                        "h-4 w-4"

                    Medium ->
                        "h-5 w-5"

                    Large ->
                        "h-6 w-6"

                    Huge ->
                        "h-7 w-7"
            ]
            []
        ]


viewTextContent : { r | text_ : String, isGhost : Bool } -> Html msg
viewTextContent { text_, isGhost } =
    Html.span
        [ classList [ ( "border-b-2 border-transparent hover:border-cyan-700 py-1 mx-1 transition-color duration-100 ease-in-out", isGhost ) ]
        ]
        [ Html.text text_ ]


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
