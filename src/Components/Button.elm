module Components.Button exposing
    ( Attribute
    , brand
    , contentWidth
    , disabled
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
    , shadow
    , size
    , small
    , tertiary
    , trailingPlacement
    )

import Components.Autocomplete exposing (Attribute)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events
import Maybe.Extra
import Utils


type alias ButtonIcon =
    { placement : IconPlacement
    , icon : FeatherIcons.Icon
    }


type alias Config msg =
    { variant : Variant
    , buttonAttributes : List (Html.Attribute msg)
    , size : Size
    , loading : Bool
    , icon : Maybe ButtonIcon
    , contentWidth : Bool
    , shadow : Bool
    }


defaultConfig : Variant -> Config msg
defaultConfig variant =
    { variant = variant
    , buttonAttributes = []
    , size = Medium
    , loading = False
    , icon = Nothing
    , contentWidth = False
    , shadow = False
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


disabled : Bool -> Attribute msg
disabled =
    attribute << Html.Attributes.disabled


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


shadow : Bool -> Attribute msg
shadow b =
    Attribute <| \c -> { c | shadow = b }


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


makeConfig : Variant -> List (Attribute msg) -> Config msg
makeConfig variant =
    Utils.getMakeConfig
        { unwrap = \(Attribute f) -> f
        , defaultConfig = defaultConfig variant
        }


type Variant
    = Primary
    | Secondary
    | Tertiary
    | Brand
    | Ghost


getSizeClass :
    Size
    ->
        { minWidth : String
        , paddingX : String
        , height : String
        , width : String
        }
getSizeClass size_ =
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


getVariantClass : Variant -> { class : String, shadowColor : Maybe String }
getVariantClass variant =
    case variant of
        Primary ->
            { class = "text-white bg-gradient-45-deg bg-[length:200%] bg-right hover:bg-left from-cyan-700 via-cyan-700 to-cyan-600"
            , shadowColor = Just "shadow-cyan-800/30  hover:shadow-cyan-800/30"
            }

        Secondary ->
            { class = "text-cyan-700 border-2 border-cyan-700 hover:shadow-md"
            , shadowColor = Nothing
            }

        Tertiary ->
            { class = "text-cyan-700 border-2 hover:border-cyan-700 hover:shadow-md"
            , shadowColor = Nothing
            }

        Brand ->
            { class = "text-white bg-gradient-45-deg bg-[length:200%] bg-right hover:bg-left from-fuchsia-800 via-fuchsia-800 to-fuchsia-600"
            , shadowColor = Just "shadow-fuchsia-800/30 hover:shadow-fuchsia-800/30"
            }

        Ghost ->
            { class = "rounded-lg text-cyan-700"
            , shadowColor = Nothing
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


normalizeConfig : Config msg -> Config msg
normalizeConfig config =
    case config.variant of
        Ghost ->
            { config
                | icon =
                    case Maybe.map .placement config.icon of
                        Just Only ->
                            Nothing

                        _ ->
                            config.icon
                , loading = False
                , contentWidth = True
            }

        _ ->
            config


view : Variant -> List (Attribute msg) -> String -> Html msg
view variant attrs text_ =
    let
        config =
            makeConfig variant attrs |> normalizeConfig

        sizeClass =
            getSizeClass config.size

        variantClass =
            getVariantClass config.variant

        shadowClass =
            case ( variantClass.shadowColor, config.shadow ) of
                ( Just color, True ) ->
                    Just ("shadow-lg hover:shadow-md " ++ color)

                _ ->
                    Nothing

        disabledClass =
            if isBackgroundFilled config.variant then
                "disabled:from-transparent disabled:to-transparent disabled:bg-neutral-200 disabled:text-neutral-500"

            else
                "disabled:border-neutral-200 disabled:text-neutral-500"
    in
    Utils.concatArgs Html.button
        [ [ class "leading-none text-sm font-semibold outline-none"
          , class "transition-all duration-200 ease-in-out"
          , class "focus:ring ring-offset-1 ring-cyan-700/20 active:scale-[0.97] disabled:active:transform-none disabled:shadow-none disabled:hover:shadow-none"
          , class variantClass.class
          , class disabledClass
          , class sizeClass.height
          , classList
                [ ( "rounded-2xl", variant /= Ghost )
                , ( sizeClass.minWidth, not config.contentWidth && not (isIconOnly config) )
                , ( sizeClass.paddingX, variant /= Ghost && not (isIconOnly config) )
                , ( sizeClass.width, isIconOnly config )
                ]
          ]
        , Maybe.Extra.mapToList class shadowClass
        , config.buttonAttributes
        ]
        [ viewButtonContent config text_
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


viewButtonContent : Config msg -> String -> Html msg
viewButtonContent config text_ =
    if config.loading then
        viewSpinner config

    else
        let
            textContent =
                Html.span [] [ Html.text text_ ]
        in
        case config.icon of
            Nothing ->
                textContent

            Just btnIcon ->
                Html.div
                    [ classList
                        [ ( "flex items-center justify-center gap-x-1.5", True )
                        , ( "border-b-2 border-transparent hover:border-cyan-700 py-1 mx-1 transition-color duration-100 ease-in-out"
                          , config.variant == Ghost
                          )
                        ]
                    ]
                    (case btnIcon.placement of
                        Only ->
                            [ viewIcon btnIcon config
                            ]

                        Leading ->
                            [ viewIcon btnIcon config
                            , textContent
                            ]

                        Trailing ->
                            [ textContent
                            , viewIcon btnIcon config
                            ]
                    )


viewIcon : ButtonIcon -> { r | size : Size, variant : Variant } -> Html msg
viewIcon btnIcon config =
    Html.i []
        [ btnIcon.icon
            |> setIconSize config.size
            |> FeatherIcons.toHtml []
        ]


viewSpinner : { r | size : Size, variant : Variant } -> Html msg
viewSpinner config =
    Html.div [ class "flex justify-center w-full" ]
        [ Html.div
            [ class "rounded-full border-2 animate-spin"
            , class <|
                if isBackgroundFilled config.variant then
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
