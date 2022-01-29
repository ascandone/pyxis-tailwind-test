module Components.TextArea exposing
    ( Attribute
    , onInput
    , placeholder
    , validation
    , value
    , view
    )

import Browser exposing (UrlRequest(..))
import Components.Internal as Internal
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Utils


type Attribute msg
    = Attribute (Config msg -> Config msg)


type alias Config msg =
    { textFieldAttributes : List (Html.Attribute msg)
    , validation : Result String ()
    , disabled : Bool
    }


defaultConfig : Config msg
defaultConfig =
    { textFieldAttributes = []
    , validation = Ok ()
    , disabled = False
    }


inputAttribute : Html.Attribute msg -> Attribute msg
inputAttribute attr =
    Attribute <| \c -> { c | textFieldAttributes = attr :: c.textFieldAttributes }


value : String -> Attribute msg
value =
    inputAttribute << Html.Attributes.value


placeholder : String -> Attribute msg
placeholder =
    inputAttribute << Html.Attributes.placeholder


validation : Result String x -> Attribute msg
validation validation_ =
    Attribute <| \c -> { c | validation = validation_ |> Result.map (\_ -> ()) }



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
    Html.div
        [ Internal.formFieldClass config
        ]
        [ Utils.concatArgs Html.textarea
            [ [ Html.Attributes.class "px-3 py-3 w-full focus:outline-none rounded-lg"
              ]
            , config.textFieldAttributes
            ]
            []
        ]
