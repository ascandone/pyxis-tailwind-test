module Components.Input exposing
    ( AddonPlacement
    , Attribute
    , Size
    , Type
    , addon
    , date
    , disabled
    , email
    , iconAddon
    , id
    , label
    , leading
    , medium
    , number
    , onBlur
    , onFocus
    , onInput
    , password
    , placeholder
    , size
    , small
    , text
    , textAddon
    , trailing
    , type_
    , validation
    , value
    , view
    )

import Components.Internal as Internal
import Components.Label as Label
import Components.Label.Internal as LabelInternal
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Html.Extra as Html
import Maybe.Extra as Maybe
import Utils


type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { inputAttributes : List (Html.Attribute msg)
    , size : Size
    , validation : Result String ()
    , disabled : Bool
    , addon : Maybe Addon
    , label : Maybe LabelInternal.Label
    , id : Maybe String
    , type_ : Type
    }


defaultConfig : Config msg
defaultConfig =
    { inputAttributes = []
    , size = Medium
    , validation = Ok ()
    , disabled = False
    , addon = Nothing
    , label = Nothing
    , id = Nothing
    , type_ = text
    }


type Type
    = Type String


unwrapType : Type -> Html.Attribute msg
unwrapType (Type t) =
    Html.Attributes.type_ t


text : Type
text =
    Type "text"


number : Type
number =
    Type "number"


email : Type
email =
    Type "email"


password : Type
password =
    Type "password"


date : Type
date =
    Type "date"


type_ : Type -> Attribute msg
type_ t =
    Attribute <| \c -> { c | type_ = t }


id : String -> Attribute msg
id id_ =
    Attribute <| \c -> { c | id = Just id_ }


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
addon placement addonType =
    Attribute <| \c -> { c | addon = Just { placement = placement, type_ = addonType } }


label : Label.Position -> Label.Type -> Attribute msg
label position addonType =
    let
        label_ =
            case addonType of
                LabelInternal.Single l1 ->
                    { position = position
                    , label = l1
                    , secondaryLabel = Nothing
                    }

                LabelInternal.Double l1 l2 ->
                    { position = position
                    , label = l1
                    , secondaryLabel = Just l2
                    }
    in
    Attribute <| \c -> { c | label = Just label_ }



-- Events


onInput : (String -> msg) -> Attribute msg
onInput =
    inputAttribute << Html.Events.onInput


onBlur : msg -> Attribute msg
onBlur =
    inputAttribute << Html.Events.onBlur


onFocus : msg -> Attribute msg
onFocus =
    inputAttribute << Html.Events.onFocus



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
    Html.div [ class "max-w-md" ]
        [ LabelInternal.view
            { size =
                case config.size of
                    Small ->
                        LabelInternal.small

                    Medium ->
                        LabelInternal.medium
            , id = config.id
            , label = config.label
            }
            (Html.div [ Internal.formFieldClass config ] <|
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
            )
        , Internal.viewValidationMessage config.validation
        ]


viewInput : Config msg -> Html msg
viewInput config =
    Utils.concatArgs Html.input
        [ [ class "w-full bg-transparent outline-none leading-none"
          , class Internal.formFieldRadiusClass
          , class <|
                case config.size of
                    Medium ->
                        "px-4 h-11"

                    Small ->
                        "px-3 h-8"
          , Html.Attributes.disabled config.disabled
          , unwrapType config.type_
          ]
        , config.inputAttributes
        , Maybe.mapToList Html.Attributes.id config.id
        ]
        []


viewAddon : Config msg -> Addon -> Html msg
viewAddon config addon_ =
    let
        commonCls =
            class <|
                Utils.stateClass
                    config.validation
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
    case addon_.type_ of
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
                    case addon_.placement of
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
                    case ( addon_.placement, config.size ) of
                        ( Leading, Small ) ->
                            "pl-3"

                        ( Leading, Medium ) ->
                            "pl-4"

                        ( Trailing, Small ) ->
                            "pr-3"

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
                    |> FeatherIcons.toHtml []
                ]
