module Examples.Validation.Main exposing (main, passwordValidation)

import Browser
import Components.Button as Btn
import Components.Input as Input
import Components.InputValidation as InputValidation
import Components.Label as Label
import Date exposing (Date)
import Email
import FormParser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Json.Encode as Enc
import Validation exposing (Validation)
import Validation.Int
import Validation.String


requiredFieldValidation : Validation String String
requiredFieldValidation =
    Validation.String.notEmpty "Required field"


nameFieldValidation : Validation String String
nameFieldValidation =
    requiredFieldValidation
        >> Result.andThen Validation.String.trim
        >> Result.andThen (Validation.String.notEmpty "Insert a valid name")


ageFieldValidation : Validation String Int
ageFieldValidation =
    requiredFieldValidation
        >> Result.andThen (Validation.String.toInt "Expected an integer")
        >> Result.andThen (Validation.Int.min 18 "Age must be >= 18")
        >> Result.andThen (Validation.Int.max 100 "Age must be <= 100")


allCharsAlpha : String -> Validation String String
allCharsAlpha =
    Validation.fromPredicate (String.all Char.isAlpha)


jobFieldValidation : Validation String String
jobFieldValidation =
    allCharsAlpha "Job cannot contain special chars"


idValidation : Validation String String
idValidation =
    requiredFieldValidation


emailFieldValidation : Validation String Email.EmailAddress
emailFieldValidation =
    Validation.String.toEmail "Insert a valid email"


passwordValidation : String -> Result String String
passwordValidation =
    Validation.String.minLength 6 "Password needs at least 6 characters"


type alias FormData =
    { name : String
    , age : Int
    , date : Date
    , job : Maybe String
    , id : Maybe String
    , email : Maybe Email.EmailAddress
    , password : String
    }


type alias Model =
    { name : InputValidation.Model String
    , age : InputValidation.Model Int
    , date : InputValidation.Model Date
    , job : InputValidation.Model (Maybe String)
    , id : InputValidation.Model (Maybe String)
    , email : InputValidation.Model (Maybe Email.EmailAddress)
    , password : InputValidation.Model String
    , confirmPassword : InputValidation.Model String
    , submittedData : List (Result String FormData)
    }


init : Model
init =
    { name = InputValidation.empty nameFieldValidation
    , age = InputValidation.empty ageFieldValidation
    , date = InputValidation.empty Date.fromIsoString
    , job = InputValidation.empty (Validation.String.optional jobFieldValidation)
    , id =
        InputValidation.init "initial-id" idValidation
            |> InputValidation.detectChanges
    , email = InputValidation.empty (Validation.String.optional emailFieldValidation)
    , password = InputValidation.empty passwordValidation
    , confirmPassword = InputValidation.empty passwordValidation
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
    | EmailInput InputValidation.Msg
    | PasswordInput InputValidation.Msg
    | ConfirmPasswordInput InputValidation.Msg
    | Submit


parseForm : Validation Model FormData
parseForm =
    FormParser.succeed FormData
        |> FormParser.input .name
        |> FormParser.input .age
        |> FormParser.input .date
        |> FormParser.input .job
        |> FormParser.input .id
        |> FormParser.input .email
        |> FormParser.input .confirmPassword


idFieldMask : String -> Maybe String
idFieldMask =
    String.replace " " "-" >> Just


idUpdate : InputValidation.GeneralMsg customMsg -> InputValidation.Model data -> InputValidation.Model data
idUpdate =
    InputValidation.enhanceUpdateWithMask idFieldMask InputValidation.update


confirmPasswordMultiValidation : Validation Model ()
confirmPasswordMultiValidation model =
    InputValidation.multiValidation2
        (\password confirmPassword ->
            if password == confirmPassword then
                Ok ()

            else
                Err "Passwords do not match"
        )
        model.password
        model.confirmPassword


afterUpdate : (Model -> Model) -> (Msg -> Model -> Model) -> Msg -> Model -> Model
afterUpdate mapper update_ msg model =
    let
        newModel =
            update_ msg model
    in
    mapper newModel


update : Msg -> Model -> Model
update =
    baseUpdate
        |> afterUpdate
            (\model ->
                { model
                    | confirmPassword =
                        InputValidation.overrideValidation
                            (confirmPasswordMultiValidation model)
                            model.confirmPassword
                }
            )


baseUpdate : Msg -> Model -> Model
baseUpdate msg model =
    case msg of
        NameInput subMsg ->
            { model | name = InputValidation.update subMsg model.name }

        AgeInput subMsg ->
            { model | age = InputValidation.update subMsg model.age }

        DateInput subMsg ->
            { model | date = InputValidation.update subMsg model.date }

        JobInput subMsg ->
            { model | job = InputValidation.update subMsg model.job }

        IdInput subMsg ->
            { model | id = idUpdate subMsg model.id }

        EmailInput subMsg ->
            { model | email = InputValidation.update subMsg model.email }

        PasswordInput subMsg ->
            { model | password = InputValidation.update subMsg model.password }

        ConfirmPasswordInput subMsg ->
            { model | confirmPassword = InputValidation.update subMsg model.confirmPassword }

        Submit ->
            model
                |> baseUpdate (AgeInput InputValidation.Submit)
                |> baseUpdate (NameInput InputValidation.Submit)
                |> baseUpdate (DateInput InputValidation.Submit)
                |> baseUpdate (JobInput InputValidation.Submit)
                |> baseUpdate (IdInput InputValidation.Submit)
                |> baseUpdate (EmailInput InputValidation.Submit)
                |> baseUpdate (PasswordInput InputValidation.Submit)
                |> baseUpdate (ConfirmPasswordInput InputValidation.Submit)
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
        , InputValidation.view model.email
            [ Input.placeholder "example@mail.com"
            , Input.label Label.vertical (Label.single "Email")
            , Input.type_ Input.email
            ]
            |> Html.map EmailInput
        , InputValidation.view model.password
            [ Input.label Label.vertical (Label.single "Password")
            , Input.type_ Input.password
            ]
            |> Html.map PasswordInput
        , InputValidation.view model.confirmPassword
            [ Input.label Label.vertical (Label.single "Confirm password")
            , Input.type_ Input.password
            ]
            |> Html.map ConfirmPasswordInput
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
        , ( "email", nullable (Email.toString >> Enc.string) formData.email )
        , ( "password", Enc.string formData.password )
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
