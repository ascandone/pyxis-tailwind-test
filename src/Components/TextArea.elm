module Components.TextArea exposing
    ( Attribute
    , disabled
    , id
    , label
    , onInput
    , placeholder
    , validation
    , value
    , view
    )

import Browser exposing (UrlRequest(..))
import Components.Internal as Internal
import Components.Label as Label
import Components.Label.Internal as LabelInternal
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Html.Extra as Html
import Utils


type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { textFieldAttributes : List (Html.Attribute msg)
    , validation : Result String ()
    , disabled : Bool
    , id : Maybe String
    , label : Maybe LabelInternal.Label
    }


defaultConfig : Config msg
defaultConfig =
    { textFieldAttributes = []
    , validation = Ok ()
    , disabled = False
    , id = Nothing
    , label = Nothing
    }


inputAttribute : Html.Attribute msg -> Attribute msg
inputAttribute attr =
    Attribute <| \c -> { c | textFieldAttributes = attr :: c.textFieldAttributes }


value : String -> Attribute msg
value =
    inputAttribute << Html.Attributes.value


id : String -> Attribute msg
id id_ =
    Attribute <| \c -> { c | id = Just id_ }


disabled : Bool -> Attribute msg
disabled disabled_ =
    Attribute <| \c -> { c | disabled = disabled_ }


placeholder : String -> Attribute msg
placeholder =
    inputAttribute << Html.Attributes.placeholder


validation : Result String x -> Attribute msg
validation validation_ =
    Attribute <| \c -> { c | validation = validation_ |> Result.map (\_ -> ()) }


label : Label.Position -> Label.Type -> Attribute msg
label position type_ =
    let
        label_ =
            case type_ of
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
            [ class <|
                case Maybe.map .position config.label of
                    Nothing ->
                        ""

                    Just LabelInternal.Vertical ->
                        "flex flex-col"

                    Just LabelInternal.Horizontal ->
                        "flex"
            ]
            [ Html.viewMaybe (viewLabel config) config.label
            , Html.div
                [ Internal.formFieldClass config
                ]
                [ Utils.concatArgs Html.textarea
                    [ [ class "px-3 py-3 w-full focus:outline-none rounded-lg"
                      , Html.Attributes.disabled config.disabled
                      ]
                    , config.textFieldAttributes
                    ]
                    []
                ]
            ]
        , Internal.viewValidationMessage config.validation
        ]


viewLabel : { r | id : Maybe String } -> LabelInternal.Label -> Html msg
viewLabel config label_ =
    LabelInternal.view
        { size = LabelInternal.medium
        , id = config.id
        , label = label_
        }
