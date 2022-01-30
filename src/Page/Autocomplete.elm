module Page.Autocomplete exposing (Model, Msg, init, update, view)

import Components.Autocomplete as Autocomplete
import Html
import Section exposing (Section)


type alias Model =
    { selectedValue : Maybe Int
    , autocompleteModel : Autocomplete.Model
    }


init : Model
init =
    { selectedValue = Nothing
    , autocompleteModel = Autocomplete.init
    }


type Msg
    = AutocompleteMsg (Autocomplete.Msg Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    { updatedModel | selectedValue = newValue }
            , Cmd.none
            )


view : Model -> List (Section Msg)
view model =
    [ Section.section "Default"
        [ Autocomplete.view model.autocompleteModel
            [ Autocomplete.selected model.selectedValue
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
