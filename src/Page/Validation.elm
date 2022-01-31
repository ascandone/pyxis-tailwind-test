module Page.Validation exposing (Model, Msg, init, update, view)

import Components.Button as Button
import Components.TextField as TextField
import Components.ValidatedTextField as ValidatedTextField
import Html
import Html.Attributes exposing (class)
import Html.Events
import Section exposing (Section)


validateName : String -> Result String String
validateName str =
    if String.length str < 5 then
        Err "Len < 5"

    else
        Ok str


validateAge : String -> Result String Int
validateAge str =
    case String.toInt str of
        Nothing ->
            Err "Expected an int"

        Just n ->
            if n < 18 then
                Err "Age must be >= 18"

            else
                Ok n


type alias FormData =
    { name : String
    , age : Int
    }


type alias Model =
    { age : ValidatedTextField.Model Int
    , name : ValidatedTextField.Model String
    , submittedData : Maybe FormData
    }


init : Model
init =
    { name = ValidatedTextField.init
    , age = ValidatedTextField.init
    , submittedData = Nothing
    }


type Msg
    = AgeTextFieldMsg ValidatedTextField.Msg
    | NameTextFieldMsg ValidatedTextField.Msg
    | Submit


parseForm : Model -> Maybe FormData
parseForm model =
    Maybe.map2 FormData
        (ValidatedTextField.getData model.name)
        (ValidatedTextField.getData model.age)


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgeTextFieldMsg subMsg ->
            { model | age = ValidatedTextField.update validateAge subMsg model.age }

        NameTextFieldMsg subMsg ->
            { model | name = ValidatedTextField.update validateName subMsg model.name }

        Submit ->
            { model | submittedData = parseForm model }


view : Model -> List (Section Msg)
view model =
    [ Section.section "Default"
        [ Html.div []
            [ Html.div [ class "h-2" ] []
            , Html.div [ class "overflow-x-auto" ]
                [ Html.text "Value: \""
                , Html.pre [ class "inline" ] [ Html.text (ValidatedTextField.getValue model.age) ]
                , Html.text "\""
                ]
            , Html.div [ class "h-2" ] []
            , Html.div [ class "overflow-x-auto" ]
                [ Html.text "Parsed value: \""
                , Html.pre [ class "inline" ] [ Html.text (Debug.toString (ValidatedTextField.getData model.age)) ]
                , Html.text "\""
                ]
            , Html.div [ class "h-2" ] []
            , Html.div [ class "overflow-x-auto" ]
                [ Html.text "FormState: \""
                , Html.pre [ class "inline" ] [ Html.text (Debug.toString model.age) ]
                , Html.text "\""
                ]
            , Html.div [ class "h-6" ] []
            , ValidatedTextField.view model.age
                [ TextField.placeholder "Età"
                , TextField.type_ TextField.number
                ]
                |> Html.map AgeTextFieldMsg
            ]
        ]
    , Section.section "Submit"
        [ Html.form [ class "space-y-6", Html.Events.onSubmit Submit ]
            [ ValidatedTextField.view model.name
                [ TextField.placeholder "Nome"
                ]
                |> Html.map NameTextFieldMsg
            , ValidatedTextField.view model.age
                [ TextField.placeholder "Età"
                ]
                |> Html.map AgeTextFieldMsg
            , Button.primary
                [ Button.size Button.large
                , Button.type_ Button.submit
                ]
                "Submit"
            ]
        , Html.hr [] []
        , Html.div [ class "overflow-x-auto" ]
            [ Html.text "Submitted data: \""
            , Html.pre [ class "inline" ] [ Html.text (Debug.toString model.submittedData) ]
            , Html.text "\""
            ]
        ]
    ]
