module Main exposing (main)

import Browser
import Components.Autocomplete as Autocomplete exposing (Model)
import Components.Button as Button
import Components.Label as Label
import Components.TextArea as TextArea
import Components.TextField as TextField
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Page
    = Button
    | TextField
    | TextArea
    | Autocomplete


type alias Model =
    { page : Page
    , autocompleteModel : Autocomplete.Model
    , autocompleteValue : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Button
      , autocompleteModel = Autocomplete.init
      , autocompleteValue = Nothing
      }
    , Cmd.none
    )


type Msg
    = SetPage Page
    | AutocompleteMsg (Autocomplete.Msg Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )

        AutocompleteMsg subMsg ->
            let
                ( newModel, evt ) =
                    Autocomplete.update subMsg model.autocompleteModel

                updatedModel =
                    { model | autocompleteModel = newModel }
            in
            ( case evt of
                Autocomplete.Noop ->
                    updatedModel

                Autocomplete.SelectionChange newValue ->
                    { updatedModel | autocompleteValue = newValue }
            , Cmd.none
            )


header : String -> Html msg
header text_ =
    Html.h3 [ Html.Attributes.class "pt-3 text-3xl tracking-wide text-gray-900 font-serif" ] [ Html.text text_ ]


viewPageLink : Page -> String -> Page -> Html Page
viewPageLink thisPage text_ currentPage =
    button
        [ class "px-2 py-2 rounded"
        , class <|
            if currentPage == thisPage then
                "bg-zinc-900 text-white"

            else
                "bg-zinc-300"
        , onClick thisPage
        ]
        [ text text_ ]


view : Model -> Html Msg
view model =
    div [ class "bg-gray-100 mb-8" ]
        [ Html.map SetPage <|
            div [ class "mx-auto max-w-screen-lg px-4 py-4 flex gap-x-4" ]
                (List.map (\c -> c model.page)
                    [ viewPageLink Button "Button"
                    , viewPageLink TextField "TextField"
                    , viewPageLink TextArea "TextArea"

                    -- , viewPageLink Autocomplete "Autocomplete"
                    ]
                )
        , div [ class "bg-white mx-auto shadow-md" ]
            [ div [ class "px-4 py-6 antialiased space-y-6 mx-auto max-w-screen-lg" ] (viewPage model)
            ]
        ]


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        Button ->
            viewButton

        TextField ->
            viewTextField

        TextArea ->
            viewTextArea

        Autocomplete ->
            viewAutocomplete model.autocompleteValue model.autocompleteModel


viewButton : List (Html msg)
viewButton =
    [ div [ class "flex flex-col gap-y-8 items-start" ]
        [ Button.primary [] "Text"
        , Button.secondary [] "Text"
        , Button.tertiary [] "Text"
        , Button.brand [] "Text"
        , Button.ghost [] "Text"
        ]
    ]


viewTextField : List (Html msg)
viewTextField =
    [ header "Default"
    , TextField.view
        [ TextField.placeholder "Input Text"
        ]
    , TextField.view
        [ TextField.value "Input Text"
        ]
    , TextField.view
        [ TextField.validation (Err "Error message")
        , TextField.placeholder "Input Text"
        ]
    , TextField.view
        [ TextField.disabled True
        , TextField.placeholder "Input Text"
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.size TextField.small
        ]
    , header "Labeled"
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.vertical (Label.single "Label")
        , TextField.id "item-id"
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.vertical (Label.double "Label" "Second label")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.horizontal (Label.single "Label")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.horizontal (Label.double "Label" "Second label")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.vertical (Label.single "Label")
        , TextField.size TextField.small
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.vertical (Label.double "Label" "Second label")
        , TextField.size TextField.small
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.horizontal (Label.single "Label")
        , TextField.size TextField.small
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.label Label.horizontal (Label.double "Label" "Second label")
        , TextField.size TextField.small
        ]
    , header "Text addon"
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.leading (TextField.textAddon "€")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.trailing (TextField.textAddon "€")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.validation (Err "Error message")
        , TextField.addon TextField.leading (TextField.textAddon "€")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.disabled True
        , TextField.addon TextField.leading (TextField.textAddon "€")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.size TextField.small
        , TextField.addon TextField.leading (TextField.textAddon "€")
        ]
    , header "Icon addon"
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.trailing (TextField.iconAddon FeatherIcons.link)
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
        , TextField.validation (Err "Error message")
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
        , TextField.disabled True
        ]
    , TextField.view
        [ TextField.placeholder "Input Text"
        , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
        , TextField.size TextField.small
        ]
    ]


viewTextArea : List (Html msg)
viewTextArea =
    [ header "Textarea"
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        ]
    , TextArea.view
        [ TextArea.value "Input Text"
        ]
    , TextArea.view
        [ TextArea.value "Input Text"
        , TextArea.validation (Err "Error message")
        ]
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        , TextArea.disabled True
        ]
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        , TextArea.label Label.horizontal (Label.single "Label")
        ]
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        , TextArea.label Label.horizontal (Label.double "Label" "Second label")
        ]
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        , TextArea.label Label.vertical (Label.single "Label")
        ]
    , TextArea.view
        [ TextArea.placeholder "Input Text"
        , TextArea.label Label.vertical (Label.double "Label" "Second label")
        ]
    ]


viewAutocomplete : Maybe Int -> Autocomplete.Model -> List (Html Msg)
viewAutocomplete selected model =
    [ Autocomplete.view model
        [ Autocomplete.selected selected
        , Autocomplete.placeholder "Autocomplete"
        ]
        [ Autocomplete.option 1 "Item option 1"
        , Autocomplete.option 2 "Item option 2"
        , Autocomplete.option 3 "Item option 3"
        , Autocomplete.option 4 "Item option 4"
        , Autocomplete.option 5 "Item option 5"
        , Autocomplete.option 6 "Item option 6"
        ]
        |> Html.map AutocompleteMsg
    ]
