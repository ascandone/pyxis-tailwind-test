module Examples.Validation.Main exposing (main)

import Browser
import Components.Button as Btn
import Components.Input as Input
import Components.InputValidation as InputValidation
import Components.Label as Label
import FormParser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Validation exposing (Validation)
import Validation.Int
import Validation.String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


validateName : String -> Result String String
validateName raw =
    Ok raw
        |> Result.andThen (Validation.String.notEmpty "Required field")


validateAge : Validation String Int
validateAge raw =
    Ok raw
        |> Result.andThen (Validation.String.notEmpty "Required field")
        |> Result.andThen (Validation.String.toInt "Expected an integer")
        |> Result.andThen (Validation.Int.min 18 "Age must be >= 18")


type alias FormData =
    { name : String
    , age : Int
    }


type alias Model =
    { name : InputValidation.Model String
    , age : InputValidation.Model Int
    , submittedData : List FormData
    }


init : Model
init =
    { name = InputValidation.empty validateName
    , age = InputValidation.empty validateAge
    , submittedData = []
    }


type Msg
    = AgeInputMsg InputValidation.Msg
    | NameInputMsg InputValidation.Msg
    | Submit


parseForm : Model -> Maybe FormData
parseForm =
    FormParser.succeed FormData
        |> FormParser.input .name
        |> FormParser.input .age


submitData : Model -> Model
submitData model =
    { model
        | submittedData =
            case parseForm model of
                Nothing ->
                    model.submittedData

                Just formData ->
                    formData :: model.submittedData
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgeInputMsg subMsg ->
            { model | age = InputValidation.update subMsg model.age }

        NameInputMsg subMsg ->
            { model | name = InputValidation.update subMsg model.name }

        Submit ->
            model
                |> update (AgeInputMsg InputValidation.Submit)
                |> update (NameInputMsg InputValidation.Submit)
                |> submitData



-- View


view : Model -> Html Msg
view model =
    Html.div [ class "px-6 py-6" ]
        [ viewForm model
        , Html.div [ class "h-4" ] []
        , Html.hr [] []
        , Html.div [ class "h-4" ] []
        , Html.ul [ class "overflow-x-auto list-disc" ]
            (model.submittedData
                |> List.map
                    (\data ->
                        Html.li []
                            [ Html.text "Submitted data: \""
                            , Html.pre [ class "inline" ] [ Html.text (Debug.toString data) ]
                            , Html.text "\""
                            ]
                    )
            )
        ]


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "space-y-6", Html.Events.onSubmit Submit ]
        [ InputValidation.view model.name
            [ Input.placeholder "John Doe"
            , Input.label Label.vertical (Label.single "Name")
            ]
            |> Html.map NameInputMsg
        , InputValidation.view model.age
            [ Input.placeholder "Age"
            , Input.label Label.vertical (Label.double "Age" "At least 18 years old")
            , Input.type_ Input.number
            ]
            |> Html.map AgeInputMsg
        , Btn.primary
            [ Btn.size Btn.large
            , Btn.type_ Btn.submit

            -- , Btn.disabled (parseForm model == Nothing)
            ]
            "Submit"
        ]
