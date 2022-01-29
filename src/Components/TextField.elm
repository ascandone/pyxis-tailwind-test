module Components.TextField exposing
    ( Attribute
    , Size
    , disabled
    , medium
    , onInput
    , placeholder
    , size
    , small
    , validation
    , value
    , view
    )

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
    }


defaultConfig : Config msg
defaultConfig =
    { inputAttributes = []
    , size = Medium
    , validation = Ok ()
    , disabled = False
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
        [ Html.div
            [ class """
            border-2 rounded-lg leading-none
            transition-all duration-200 ease-in-out
            """
            , class <|
                case ( config.validation, config.disabled ) of
                    ( _, True ) ->
                        "bg-neutral-100 border-neutral-200 placeholder:text-neutral-400"

                    ( Ok (), _ ) ->
                        "focus-within:border-cyan-600 hover:border-cyan-600 focus-within:ring ring-cyan-200 text-gray-900"

                    ( Err _, _ ) ->
                        "border-red-500  focus-within:ring ring-red-200 text-red-800 placeholder:text-red-200"
            ]
            [ Utils.concatArgs Html.input
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
            ]
        , case config.validation of
            Ok () ->
                Html.text ""

            Err validationMsg ->
                Html.span [ class "text-xs text-red-800 font-medium" ] [ Html.text validationMsg ]
        ]
