module Components.Internal exposing (formFieldClass, formFieldRadiusClass, formFieldTransitionClass, viewValidationMessage)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, classList)
import Utils


formFieldTransitionClass : String
formFieldTransitionClass =
    "duration-200 ease-in-out"


formFieldRadiusClass : String
formFieldRadiusClass =
    "rounded-lg"


formFieldClass : { r | validation : Result error (), disabled : Bool } -> Attribute msg
formFieldClass { validation, disabled } =
    let
        stateClass =
            Utils.stateClass validation disabled
    in
    classList
        [ ( "group flex-1 flex border-2 leading-none transition-all", True )
        , ( formFieldRadiusClass, True )
        , ( formFieldTransitionClass, True )
        , ( stateClass
                { default = "focus-within:border-cyan-600 hover:border-cyan-600 focus-within:ring ring-cyan-200 text-gray-900"
                , disabled = "bg-neutral-100 border-neutral-200 placeholder:text-neutral-400"
                , error = "border-red-500  focus-within:ring ring-red-200 text-red-800 placeholder:text-red-200"
                }
          , True
          )
        ]


viewValidationMessage : Result String () -> Html msg
viewValidationMessage validation_ =
    case validation_ of
        Ok () ->
            Html.text ""

        Err validationMsg ->
            Html.span [ class "text-xs text-red-800 font-medium" ]
                [ Html.text validationMsg ]
