module Components.InputValidation exposing
    ( Model
    , Msg(..)
    , ValidationMessageStrategy
    , detectChanges
    , empty
    , field
    , getData
    , getValue
    , init
    , update
    , updateWithCustomStrategy
    , validate
    , validateOnBlurStrategy
    , view
    )

import Components.Input as Input
import FormState exposing (FormState)
import Html exposing (Html)
import Utils
import Validation exposing (Validation)


{-| Non-opaque by design
Keep variants exposed
-}
type Msg
    = Focus
    | Input String
    | Blur
    | Submit


type Model data
    = Model
        { formState : FormState
        , value : String
        , initialValue : String
        , validation : Validation String data
        , showValidation : Bool
        }


init : String -> Validation String data -> Model data
init initialValue validation =
    Model
        { formState = FormState.Untouched
        , value = initialValue
        , initialValue = initialValue
        , validation = validation
        , showValidation = False
        }


detectChanges : Model data -> Model (Maybe data)
detectChanges (Model model) =
    let
        validation str =
            if str == model.initialValue then
                Ok Nothing

            else
                Result.map Just (model.validation str)
    in
    Model
        { formState = model.formState
        , value = model.value
        , showValidation = model.showValidation
        , initialValue = model.initialValue
        , validation = validation
        }


empty : Validation String data -> Model data
empty =
    init ""


getValue : Model data -> String
getValue (Model { value }) =
    value


getData : Model data -> Maybe data
getData (Model { validation, value }) =
    Result.toMaybe (validation value)


validate : Model data -> Result String data
validate (Model { validation, value }) =
    validation value


type alias ValidationMessageStrategy data =
    { formState : FormState
    , msg : Msg
    , previousValidation : Result String data
    }
    -> Maybe Bool


updateWithCustomStrategy : ValidationMessageStrategy data -> Msg -> Model data -> Model data
updateWithCustomStrategy strategy msg (Model model) =
    let
        newModel =
            case msg of
                Input str ->
                    { model
                        | value = str
                        , formState = FormState.input model.formState
                    }

                Blur ->
                    { model
                        | formState = FormState.blur model.formState
                    }

                Focus ->
                    { model
                        | formState = FormState.focus model.formState
                    }

                Submit ->
                    model
    in
    Model
        { newModel
            | showValidation =
                case newModel.validation newModel.value of
                    Ok _ ->
                        False

                    Err _ ->
                        strategy
                            { formState = newModel.formState
                            , msg = msg
                            , previousValidation = model.validation model.value
                            }
                            |> Maybe.withDefault newModel.showValidation
        }


update : Msg -> Model data -> Model data
update =
    updateWithCustomStrategy validateOnBlurStrategy


view : Model x -> List (Input.Attribute Msg) -> Html Msg
view (Model model) attrs =
    Utils.concatArgs Input.view
        [ attrs
        , [ Input.value model.value
          , Input.onInput Input
          , Input.onBlur Blur
          , Input.onFocus Focus
          , if model.showValidation then
                Input.validation (model.validation model.value)

            else
                Input.validation (Ok ())
          ]
        ]



-- Default strategies


validateOnBlurStrategy : ValidationMessageStrategy value
validateOnBlurStrategy { formState, msg, previousValidation } =
    case ( formState, msg, previousValidation ) of
        ( _, Blur, _ ) ->
            Just True

        ( FormState.Touched { blurredAtLeastOnce }, Input _, Err _ ) ->
            if blurredAtLeastOnce then
                Just True

            else
                Nothing

        ( _, Submit, _ ) ->
            Just True

        _ ->
            Nothing



-- Experimental


field : form -> (form -> Model data) -> Maybe (data -> a) -> Maybe a
field form getter =
    Maybe.map2 (|>) (getData (getter form))
