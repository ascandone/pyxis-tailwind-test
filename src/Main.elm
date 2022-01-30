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
import Section exposing (Section)


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
    Html.h3 [ Html.Attributes.class "text-2xl tracking-wide text-gray-900 font-serif" ] [ Html.text text_ ]


viewPageLink : Page -> String -> Page -> Html Page
viewPageLink thisPage text_ currentPage =
    button
        [ class "px-4 py-2 leading-none rounded"
        , class <|
            if currentPage == thisPage then
                "bg-zinc-900 text-white"

            else
                "bg-zinc-200"
        , onClick thisPage
        ]
        [ text text_ ]


view : Model -> Html Msg
view model =
    div []
        [ Html.map SetPage <|
            div [ class "mx-auto max-w-screen-lg px-4 py-4 flex gap-x-4" ]
                (List.map (\c -> c model.page)
                    [ viewPageLink Button "Button"
                    , viewPageLink TextField "TextField"
                    , viewPageLink TextArea "TextArea"
                    ]
                )
        , viewPage model
        , div [ class "h-10" ] []
        ]


section : String -> List (Html msg) -> Html msg
section header_ children =
    Html.section [ class "bg-white rounded shadow-soft px-6 pt-2 pb-4 " ]
        [ header header_
        , div [ class "h-4" ] []
        , div [ class "space-y-4" ] children
        ]


viewPage : Model -> Html Msg
viewPage model =
    Section.view <|
        case model.page of
            Button ->
                buttonSections

            TextField ->
                textFieldSections

            TextArea ->
                textAreaSections

            Autocomplete ->
                autocompleteSections model.autocompleteValue model.autocompleteModel


buttonSections : List (Section msg)
buttonSections =
    let
        btnGroup =
            div [ class "flex gap-x-8 gap-y-3 items-start flex-wrap" ]
    in
    [ Section.section "Primary"
        [ btnGroup
            [ Button.primary [ Button.size Button.small ] "Text"
            , Button.primary [ Button.size Button.medium ] "Text"
            , Button.primary [ Button.size Button.large ] "Text"
            , Button.primary [ Button.size Button.huge ] "Text"
            , Button.primary [ Button.loading True ] "Text"
            ]
        , btnGroup
            [ Button.primary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Secondary"
        [ btnGroup
            [ Button.secondary [ Button.size Button.small ] "Text"
            , Button.secondary [ Button.size Button.medium ] "Text"
            , Button.secondary [ Button.size Button.large ] "Text"
            , Button.secondary [ Button.size Button.huge ] "Text"
            ]
        , btnGroup
            [ Button.secondary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Tertiary"
        [ btnGroup
            [ Button.tertiary [ Button.size Button.small ] "Text"
            , Button.tertiary [ Button.size Button.medium ] "Text"
            , Button.tertiary [ Button.size Button.large ] "Text"
            , Button.tertiary [ Button.size Button.huge ] "Text"
            , Button.tertiary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Brand"
        [ btnGroup
            [ Button.brand [ Button.size Button.small ] "Text"
            , Button.brand [ Button.size Button.medium ] "Text"
            , Button.brand [ Button.size Button.large ] "Text"
            , Button.brand [ Button.size Button.huge ] "Text"
            , Button.brand [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Ghost"
        [ btnGroup
            [ Button.ghost [ Button.size Button.small ] "Text"
            , Button.ghost [ Button.size Button.medium ] "Text"
            , Button.ghost [ Button.size Button.large ] "Text"
            , Button.ghost [ Button.size Button.huge ] "Text"
            ]
        ]
    , Section.section "Loading"
        [ btnGroup
            [ Button.primary [ Button.loading True, Button.size Button.small ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.medium ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.large ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.huge ] "Text"
            ]
        ]
    ]


textFieldSections : List (Section msg)
textFieldSections =
    [ Section.section "Default"
        [ TextField.view
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
        ]
    , Section.section "Labeled"
        [ TextField.view
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
        ]
    , Section.section "Text addon"
        [ TextField.view
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
        ]
    , Section.section "Icon addon"
        [ TextField.view
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
    ]


textAreaSections : List (Section msg)
textAreaSections =
    [ Section.section "Default"
        [ TextArea.view
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
    ]


autocompleteSections : Maybe Int -> Autocomplete.Model -> List (Section Msg)
autocompleteSections selected model =
    [ Section.section "Default"
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
    ]
