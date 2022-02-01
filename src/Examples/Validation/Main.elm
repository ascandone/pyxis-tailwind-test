module Examples.Validation.Main exposing (main)

import Browser
import Components.Button as Btn
import Components.Input as Input
import Components.InputValidation as InputValidation
import Components.Label as Label
import Date exposing (Date)
import FormParser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Json.Encode as Enc
import Validation exposing (Validation)
import Validation.Int
import Validation.String


nameValidation : Validation String String
nameValidation raw =
    Ok raw
        |> Result.andThen (Validation.String.notEmpty "Required field")
        |> Result.andThen Validation.String.trim
        |> Result.andThen (Validation.String.notEmpty "Insert a valid name")


ageValidation : Validation String Int
ageValidation raw =
    Ok raw
        |> Result.andThen (Validation.String.notEmpty "Required field")
        |> Result.andThen (Validation.String.toInt "Expected an integer")
        |> Result.andThen (Validation.Int.min 18 "Age must be >= 18")
        |> Result.andThen (Validation.Int.max 100 "Age must be <= 100")


allCharsAlpha : String -> Validation String String
allCharsAlpha =
    Validation.fromPredicate (String.all Char.isAlpha)


jobValidation : Validation String String
jobValidation =
    allCharsAlpha "Job cannot contain special chars"


idValidation : Validation String String
idValidation =
    Validation.String.notEmpty "Required field"


type alias FormData =
    { name : String
    , age : Int
    , date : Date
    , job : Maybe String
    , id : Maybe String
    }


type alias Model =
    { name : InputValidation.Model String
    , age : InputValidation.Model Int
    , date : InputValidation.Model Date
    , job : InputValidation.Model (Maybe String)
    , id : InputValidation.Model (Maybe String)
    , submittedData : List (Result String FormData)
    }


init : Model
init =
    { name = InputValidation.empty nameValidation
    , age = InputValidation.empty ageValidation
    , date = InputValidation.empty Date.fromIsoString
    , job = InputValidation.empty (Validation.String.optional jobValidation)
    , id =
        InputValidation.init "initial-id" idValidation
            |> InputValidation.detectChanges
    , submittedData = []
    }


type IdInputMsg
    = KeyDown { keyCode : Int }


type Msg
    = AgeInput InputValidation.Msg
    | NameInput InputValidation.Msg
    | DateInput InputValidation.Msg
    | JobInput InputValidation.Msg
    | IdInput (InputValidation.GeneralMsg IdInputMsg)
    | Submit


parseForm : Validation Model FormData
parseForm =
    FormParser.succeed FormData
        |> FormParser.input .name
        |> FormParser.input .age
        |> FormParser.input .date
        |> FormParser.input .job
        |> FormParser.input .id


update : Msg -> Model -> Model
update msg model =
    case msg of
        AgeInput subMsg ->
            { model | age = InputValidation.update subMsg model.age }

        NameInput subMsg ->
            { model | name = InputValidation.update subMsg model.name }

        DateInput subMsg ->
            { model | date = InputValidation.update subMsg model.date }

        JobInput subMsg ->
            { model | job = InputValidation.update subMsg model.job }

        IdInput subMsg ->
            { model | id = InputValidation.update subMsg model.id }

        Submit ->
            model
                |> update (AgeInput InputValidation.Submit)
                |> update (NameInput InputValidation.Submit)
                |> update (DateInput InputValidation.Submit)
                |> update (JobInput InputValidation.Submit)
                |> update (IdInput InputValidation.Submit)
                |> submitData


submitData : Model -> Model
submitData model =
    { model | submittedData = parseForm model :: model.submittedData }



-- View


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ class "space-y-6", Html.Events.onSubmit Submit ]
        [ InputValidation.view model.name
            [ Input.placeholder "John Doe"
            , Input.label Label.vertical (Label.double "Name" "Gets trimmed")
            ]
            |> Html.map NameInput
        , InputValidation.view model.age
            [ Input.placeholder "Age"
            , Input.label Label.vertical (Label.double "Age" "At least 18 years old")
            , Input.type_ Input.number
            ]
            |> Html.map AgeInput
        , InputValidation.view model.date
            [ Input.label Label.vertical (Label.single "Birth date")
            , Input.type_ Input.date
            ]
            |> Html.map DateInput
        , InputValidation.view model.job
            [ Input.placeholder "Job (alpha chars only)"
            , Input.label Label.vertical (Label.double "Job" "Optional field")
            ]
            |> Html.map JobInput
        , InputValidation.view model.id
            [ Input.placeholder "Id"
            , Input.label Label.vertical (Label.double "Id" "Null, unless different from default value")
            , Input.onKeyDown (InputValidation.Custom << KeyDown)
            ]
            |> Html.map IdInput
        , Btn.primary
            [ Btn.size Btn.large
            , Btn.type_ Btn.submit
            ]
            "Submit"
        ]



-- Boilerplate


nullable : (a -> Enc.Value) -> Maybe a -> Enc.Value
nullable f m =
    case m of
        Nothing ->
            Enc.null

        Just x ->
            f x


encodeForm : FormData -> Enc.Value
encodeForm formData =
    Enc.object
        [ ( "name", Enc.string formData.name )
        , ( "age", Enc.int formData.age )
        , ( "date", Enc.string (Date.toIsoString formData.date) )
        , ( "job", nullable Enc.string formData.job )
        , ( "id", nullable Enc.string formData.id )
        ]


encodeFormDataResult : Result String FormData -> Enc.Value
encodeFormDataResult res =
    case res of
        Err err ->
            Enc.string ("ERR: " ++ err)

        Ok d ->
            encodeForm d


view : Model -> Html Msg
view model =
    Html.div [ class "px-6 py-6" ]
        [ viewForm model
        , Html.div [ class "h-4" ] []
        , Html.hr [] []
        , Html.div [ class "h-4" ] []
        , Html.text "Submitted data: "
        , Html.ul [ class "overflow-x-auto list-disc" ]
            (model.submittedData
                |> List.map
                    (\data ->
                        Html.li []
                            [ Html.pre [ class "inline" ] [ Html.text (Enc.encode 2 (encodeFormDataResult data)) ]
                            ]
                    )
            )
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
