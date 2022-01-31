module Examples.Validation.Main exposing (main)

import Browser
import Components.Button as Btn
import Components.Input as Input
import Components.InputValidation as InputValidation
import FormParser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


validateName : String -> Result String String
validateName str =
    if String.length str < 2 then
        Err "Len < 2"

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
    { age : InputValidation.Model Int
    , name : InputValidation.Model String
    , submittedData : List FormData
    }


init : Model
init =
    { name = InputValidation.init
    , age = InputValidation.init
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgeInputMsg subMsg ->
            { model | age = InputValidation.update validateAge subMsg model.age }

        NameInputMsg subMsg ->
            { model | name = InputValidation.update validateName subMsg model.name }

        Submit ->
            { model
                | submittedData =
                    case parseForm model of
                        Nothing ->
                            model.submittedData

                        Just formData ->
                            formData :: model.submittedData
            }



-- View


view : Model -> Html Msg
view model =
    Html.div [ class "px-6 py-2" ]
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
            [ Input.placeholder "Nome"
            ]
            |> Html.map NameInputMsg
        , InputValidation.view model.age
            [ Input.placeholder "Età"
            , Input.type_ Input.number
            ]
            |> Html.map AgeInputMsg
        , Btn.primary
            [ Btn.size Btn.large
            , Btn.type_ Btn.submit
            , Btn.disabled (parseForm model == Nothing)
            ]
            "Submit"
        ]
