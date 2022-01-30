module Components.Button exposing
    ( Attribute
    , brand
    , contentWidth
    , ghost
    , huge
    , icon
    , large
    , leadingPlacement
    , loading
    , medium
    , onBlur
    , onClick
    , onFocus
    , only
    , primary
    , secondary
    , size
    , small
    , tertiary
    , trailingPlacement
    )

import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events
import Utils


type alias ButtonIcon =
    { placement : IconPlacement
    , icon : FeatherIcons.Icon
    }


type alias Config msg =
    { buttonAttributes : List (Html.Attribute msg)
    , size : Size
    , loading : Bool
    , icon : Maybe ButtonIcon
    , contentWidth : Bool
    }


defaultConfig : Config msg
defaultConfig =
    { buttonAttributes = []
    , size = Medium
    , loading = False
    , icon = Nothing
    , contentWidth = False
    }


type Attribute msg
    = Attribute (Config msg -> Config msg)


loading : Bool -> Attribute msg
loading loading_ =
    Attribute <| \c -> { c | loading = loading_ }


contentWidth : Bool -> Attribute msg
contentWidth b =
    Attribute <| \c -> { c | contentWidth = b }


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


type IconPlacement
    = Leading
    | Trailing
    | Only


leadingPlacement : IconPlacement
leadingPlacement =
    Leading


trailingPlacement : IconPlacement
trailingPlacement =
    Trailing


only : IconPlacement
only =
    Only


icon : IconPlacement -> FeatherIcons.Icon -> Attribute msg
icon placement icon_ =
    Attribute <| \c -> { c | icon = Just { placement = placement, icon = icon_ } }


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


sizeClass :
    Size
    ->
        { minWidth : String
        , paddingX : String
        , height : String
        , width : String
        }
sizeClass size_ =
    case size_ of
        Small ->
            { minWidth = "min-w-[5rem]"
            , paddingX = "px-4"
            , height = "h-6"
            , width = "w-6"
            }

        Medium ->
            { minWidth = "min-w-[7.5rem]"
            , paddingX = "px-5"
            , height = "h-8"
            , width = "w-8"
            }

        Large ->
            { minWidth = "min-w-[10rem]"
            , paddingX = "px-6"
            , height = "h-10"
            , width = "w-10"
            }

        Huge ->
            { minWidth = "min-w-[13.75rem]"
            , paddingX = "px-7"
            , height = "h-14"
            , width = "w-14"
            }


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


isIconOnly : { r | icon : Maybe ButtonIcon } -> Bool
isIconOnly config =
    case Maybe.map .placement config.icon of
        Just Only ->
            True

        _ ->
            False


normalizeConfig : Variant -> Config msg -> Config msg
normalizeConfig variant config =
    case variant of
        Ghost ->
            { config
                | icon = Nothing
                , loading = False
                , contentWidth = True
            }

        _ ->
            config


view : Variant -> List (Attribute msg) -> String -> Html msg
view variant attrs text_ =
    let
        config =
            makeConfig attrs |> normalizeConfig variant

        sizeCls =
            sizeClass config.size
    in
    Utils.concatArgs Html.button
        [ [ class "leading-none text-sm font-semibold outline-none"
          , class "transition-all duration-200 ease-in-out"
          , class "focus:ring ring-offset-1 ring-cyan-700/20 active:scale-[0.97]"
          , class sizeCls.height
          , classList
                [ ( "rounded-2xl", variant /= Ghost )
                , ( sizeCls.minWidth, not config.contentWidth && not (isIconOnly config) )
                , ( sizeCls.paddingX, variant /= Ghost && not (isIconOnly config) )
                , ( sizeCls.width, isIconOnly config )
                ]
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
        , config.buttonAttributes
        ]
        [ viewButtonContent variant config text_
        ]


setIconSize : Size -> FeatherIcons.Icon -> FeatherIcons.Icon
setIconSize size_ =
    FeatherIcons.withSize <|
        case size_ of
            Small ->
                14

            Medium ->
                18

            Large ->
                20

            Huge ->
                24


viewButtonContent : Variant -> Config msg -> String -> Html msg
viewButtonContent variant config text_ =
    if config.loading then
        viewSpinner variant config

    else
        let
            textContent =
                viewTextContent
                    { isGhost = variant == Ghost
                    , text_ = text_
                    }
        in
        case config.icon of
            Nothing ->
                textContent

            Just btnIcon ->
                Html.div [ class "flex items-center justify-center gap-x-1" ] <|
                    case btnIcon.placement of
                        Only ->
                            [ viewIcon variant btnIcon config
                            ]

                        Leading ->
                            [ viewIcon variant btnIcon config
                            , textContent
                            ]

                        Trailing ->
                            [ textContent
                            , viewIcon variant btnIcon config
                            ]


viewIcon : Variant -> ButtonIcon -> { r | size : Size } -> Html msg
viewIcon variant btnIcon config =
    Html.i
        [ class <|
            if isBackgroundFilled variant then
                "text-white"

            else
                "text-cyan-700/90"
        ]
        [ btnIcon.icon
            |> setIconSize config.size
            |> FeatherIcons.toHtml []
        ]


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
