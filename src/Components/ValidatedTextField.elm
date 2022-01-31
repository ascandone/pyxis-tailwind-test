module Components.ValidatedTextField exposing
    ( Model
    , Msg(..)
    , ValidationStrategy
    , getData
    , getValue
    , init
    , initWithInitial
    , update
    , updateWithCustomStrategy
    , validateOnBlurStrategy
    , view
    , withValue
    )

import Components.TextField as TextField
import FormState exposing (FormState)
import Html exposing (Html)
import Utils


{-| Non-opaque by design
Keep variants exposed
-}
type Msg
    = Focus
    | Input String
    | Blur


type Model data
    = Model
        { formState : FormState
        , value : String
        , initialValue : String
        , validation : Maybe (Result String data)
        }


initWithInitial : String -> Model data
initWithInitial intialValue =
    Model
        { formState = FormState.Untouched
        , value = intialValue
        , initialValue = intialValue
        , validation = Nothing
        }


init : Model data
init =
    initWithInitial ""


getValue : Model data -> String
getValue (Model { value }) =
    value


getData : Model data -> Maybe data
getData (Model { validation }) =
    Maybe.andThen Result.toMaybe validation


withValue : String -> Model data -> Model data
withValue value (Model data) =
    Model { data | value = value }


type alias ValidationStrategy data =
    { formState : FormState
    , msg : Msg
    , currentValidation : Maybe (Result String data)
    , runValidation : () -> Result String data
    }
    -> Maybe (Result String data)


refreshValidation :
    { validate : String -> Result String data
    , validationStrategy : ValidationStrategy data
    , msg : Msg
    }
    -> Model data
    -> Model data
refreshValidation { validate, validationStrategy, msg } (Model model) =
    Model
        { model
            | validation =
                validationStrategy
                    { formState = model.formState
                    , msg = msg
                    , currentValidation = model.validation
                    , runValidation = \() -> validate model.value
                    }
        }


updateWithCustomStrategy : ValidationStrategy data -> (String -> Result String data) -> Msg -> Model data -> Model data
updateWithCustomStrategy strategy validate msg (Model model) =
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
    in
    refreshValidation
        { validate = validate
        , validationStrategy = strategy
        , msg = msg
        }
        (Model newModel)


update : (String -> Result String data) -> Msg -> Model data -> Model data
update =
    updateWithCustomStrategy validateOnBlurStrategy


getValidation : Maybe (Result String x) -> Result String ()
getValidation maybeResult =
    case maybeResult of
        Nothing ->
            Ok ()

        Just res ->
            Result.map (\_ -> ()) res


view : Model x -> List (TextField.Attribute Msg) -> Html Msg
view (Model model) attrs =
    Utils.concatArgs TextField.view
        [ attrs
        , [ TextField.value model.value
          , TextField.onInput Input
          , TextField.onBlur Blur
          , TextField.onFocus Focus
          , TextField.validation (getValidation model.validation)
          ]
        ]



-- Default strategies


validateOnBlurStrategy : ValidationStrategy value
validateOnBlurStrategy { formState, msg, currentValidation, runValidation } =
    case ( formState, msg, currentValidation ) of
        ( FormState.Touched data, Blur, _ ) ->
            if data.dirty then
                Just (runValidation ())

            else
                currentValidation

        ( _, Blur, _ ) ->
            Just (runValidation ())

        ( _, Input _, Just (Err _) ) ->
            Just (runValidation ())

        _ ->
            currentValidation
