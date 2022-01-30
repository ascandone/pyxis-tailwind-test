module Components.Autocomplete exposing
    ( Attribute
    , Event(..)
    , Model
    , Msg
    , Option
    , init
    , option
    , placeholder
    , selected
    , update
    , view
    )

import Components.Internal as Internal
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Utils


type Event value
    = Noop
    | SelectionChange (Maybe value)


type Model
    = Model
        { focused : Bool
        , value : String
        }


init : Model
init =
    Model
        { focused = False
        , value = ""
        }


type Msg value
    = Focus
    | Blur
    | Input String
    | Selected value
    | ClearSelection


type alias Config value =
    { inputAttributes : List (Html.Attribute (Msg value))
    , selected : Maybe value
    }


defaultConfig : Config value
defaultConfig =
    { inputAttributes = []
    , selected = Nothing
    }


type Attribute value
    = Attribute (Config value -> Config value)


selected : Maybe value -> Attribute value
selected selectedValue =
    Attribute <| \c -> { c | selected = selectedValue }


inputAttribute : Html.Attribute (Msg value) -> Attribute value
inputAttribute attr =
    Attribute <| \c -> { c | inputAttributes = attr :: c.inputAttributes }


placeholder : String -> Attribute value
placeholder =
    inputAttribute << Html.Attributes.placeholder


makeConfig : List (Attribute value) -> Config value
makeConfig =
    Utils.getMakeConfig
        { unwrap = \(Attribute f) -> f
        , defaultConfig = defaultConfig
        }


type Option a
    = Option
        { value : a
        , text : String
        }


option : a -> String -> Option a
option value text_ =
    Option
        { value = value
        , text = text_
        }


update : Msg value -> Model -> ( Model, Event value )
update msg (Model model) =
    case msg of
        Input str ->
            ( Model { model | value = str }
            , Noop
            )

        Focus ->
            ( Model { model | focused = True }
            , Noop
            )

        Blur ->
            ( Model { model | focused = False }
            , Noop
            )

        Selected value ->
            ( Model model
            , SelectionChange (Just value)
            )

        ClearSelection ->
            ( Model model
            , SelectionChange Nothing
            )


view : Model -> List (Attribute value) -> List (Option value) -> Html (Msg value)
view (Model model) attrs options =
    let
        config =
            makeConfig attrs
    in
    Html.div [ class "" ]
        [ Html.div
            [ Internal.formFieldClass
                { validation = Ok ()
                , disabled = False
                }
            , class "max-w-md relative"
            ]
            [ Html.div [ class "flex flex-col w-full overflow-hidden" ]
                [ Utils.concatArgs Html.input
                    [ [ class Internal.formFieldRadiusClass
                      , class "px-3 py-3 w-full focus:outline-none"
                      , Html.Events.onFocus Focus
                      , Html.Events.onBlur Blur
                      , Html.Events.onInput Input
                      ]
                    , config.inputAttributes
                    ]
                    []
                , Html.ul
                    [ class "overflow-y-scroll transition-all duration-200 ease-in-out px-2 w-full"
                    , class <|
                        if model.value /= "" && model.focused then
                            "h-48 py-2 border-t"

                        else
                            "h-0"
                    ]
                    (options
                        |> List.map
                            (\(Option option_) ->
                                let
                                    isSelected =
                                        config.selected == Just option_.value
                                in
                                Html.li []
                                    [ Html.button
                                        [ class "px-2 py-4 leading-none rounded-md cursor-pointer w-full text-left"
                                        , class <|
                                            if isSelected then
                                                "text-cyan-800 bg-cyan-700 bg-opacity-10"

                                            else
                                                "text-gray-600"
                                        , Html.Events.onMouseDown (Selected option_.value)
                                        ]
                                        [ Html.text option_.text ]
                                    ]
                            )
                    )
                ]
            , Html.div [ class "absolute right-0 top-0 bottom-0 mr-2" ]
                [ Html.div [ class "flex items-center " ]
                    [ case config.selected of
                        Nothing ->
                            searchIcon

                        Just _ ->
                            crossIconButton
                    ]
                ]
            ]
        ]


searchIcon : Html msg
searchIcon =
    Html.div [ class "p-1" ]
        [ FeatherIcons.search
            |> FeatherIcons.toHtml [ class "fill-gray-600" ]
        ]


crossIconButton : Html (Msg value)
crossIconButton =
    Html.button
        [ class "bg-neutral-100 rounded-full p-1 text-gray-800"
        , Html.Events.onClick ClearSelection
        ]
        [ FeatherIcons.x
            |> FeatherIcons.toHtml [ class "fill-current" ]
        ]
