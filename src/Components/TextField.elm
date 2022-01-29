module Components.TextField exposing
    ( AddonPlacement
    , Attribute
    , Size
    , addon
    , disabled
    , iconAddon
    , leading
    , medium
    , onInput
    , placeholder
    , size
    , small
    , textAddon
    , trailing
    , validation
    , value
    , view
    )

import Components.Internal as Internal
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Utils


type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { inputAttributes : List (Html.Attribute msg)
    , size : Size
    , validation : Result String ()
    , disabled : Bool
    , addon : Maybe Addon
    }


defaultConfig : Config msg
defaultConfig =
    { inputAttributes = []
    , size = Medium
    , validation = Ok ()
    , disabled = False
    , addon = Nothing
    }


inputAttribute : Html.Attribute msg -> Attribute msg
inputAttribute attr =
    Attribute <| \c -> { c | inputAttributes = attr :: c.inputAttributes }


value : String -> Attribute msg
value =
    inputAttribute << Html.Attributes.value


placeholder : String -> Attribute msg
placeholder =
    inputAttribute << Html.Attributes.placeholder


validation : Result String x -> Attribute msg
validation validation_ =
    Attribute <| \c -> { c | validation = validation_ |> Result.map (\_ -> ()) }


type Size
    = Small
    | Medium


small : Size
small =
    Small


medium : Size
medium =
    Medium


size : Size -> Attribute msg
size size_ =
    Attribute <| \c -> { c | size = size_ }


disabled : Bool -> Attribute msg
disabled disabled_ =
    Attribute <| \c -> { c | disabled = disabled_ }


type AddonPlacement
    = Trailing
    | Leading


trailing : AddonPlacement
trailing =
    Trailing


leading : AddonPlacement
leading =
    Leading


type AddonType
    = IconAddon FeatherIcons.Icon
    | TextAddon String


iconAddon : FeatherIcons.Icon -> AddonType
iconAddon =
    IconAddon


textAddon : String -> AddonType
textAddon =
    TextAddon


type alias Addon =
    { placement : AddonPlacement
    , type_ : AddonType
    }


addon : AddonPlacement -> AddonType -> Attribute msg
addon placement type_ =
    Attribute <| \c -> { c | addon = Just { placement = placement, type_ = type_ } }



-- Events


onInput : (String -> msg) -> Attribute msg
onInput =
    inputAttribute << Html.Events.onInput



-- View


makeConfig : List (Attribute msg) -> Config msg
makeConfig =
    Utils.getMakeConfig
        { unwrap = \(Attribute f) -> f
        , defaultConfig = defaultConfig
        }


view : List (Attribute msg) -> Html msg
view attrs =
    let
        config =
            makeConfig attrs
    in
    Html.div []
        [ Html.div [ Internal.formFieldClass config ] <|
            case config.addon of
                Nothing ->
                    [ viewInput config ]

                Just addon_ ->
                    case addon_.placement of
                        Leading ->
                            [ viewAddon config addon_
                            , viewInput config
                            ]

                        Trailing ->
                            [ viewInput config
                            , viewAddon config addon_
                            ]
        , case config.validation of
            Ok () ->
                Html.text ""

            Err validationMsg ->
                Html.span [ class "text-xs text-red-800 font-medium" ] [ Html.text validationMsg ]
        ]


viewInput : Config msg -> Html msg
viewInput config =
    Utils.concatArgs Html.input
        [ [ class """
                w-full rounded-lg bg-transparent
                outline-none
                """
          , class <|
                case config.size of
                    Medium ->
                        "px-4 py-4"

                    Small ->
                        "px-2 py-2"
          , Html.Attributes.disabled config.disabled
          ]
        , config.inputAttributes
        ]
        []


viewAddon : Config msg -> Addon -> Html msg
viewAddon config { type_, placement } =
    let
        commonCls =
            Utils.stateClass config.validation
                config.disabled
                { default = """
                        group-hover:border-cyan-600 group-focus-within:border-cyan-600
                        group-focus-within:text-cyan-600 group-hover:text-cyan-600
                        text-gray-500
                    """
                , disabled = "text-gray-400 border-neutral-300"
                , error = """
                        group-hover:border-red-600 group-focus-within:border-red-600
                        group-focus-within:text-red-600 group-hover:text-red-600
                        text-red-500
                    """
                }
    in
    case type_ of
        TextAddon text_ ->
            Html.div
                [ class "transition-color flex items-center"
                , class <|
                    case config.size of
                        Small ->
                            "px-3"

                        Medium ->
                            "px-4"
                , commonCls
                , class Internal.formFieldTransitionClass
                , class <|
                    case placement of
                        Leading ->
                            "border-r"

                        Trailing ->
                            "border-l"
                ]
                [ Html.text text_
                ]

        IconAddon icon ->
            Html.span
                [ class "flex items-center"
                , class <|
                    case ( placement, config.size ) of
                        ( Leading, Small ) ->
                            "pl-2"

                        ( Leading, Medium ) ->
                            "pl-4"

                        ( Trailing, Small ) ->
                            "pr-2"

                        ( Trailing, Medium ) ->
                            "pr-4"
                , commonCls
                ]
                [ icon
                    |> FeatherIcons.withSize
                        (case config.size of
                            Small ->
                                18

                            Medium ->
                                22
                        )
                    |> FeatherIcons.toHtml
                        [ class "fill-current"
                        ]
                ]
