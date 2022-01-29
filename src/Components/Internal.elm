module Components.Internal exposing (formFieldClass, formFieldTransitionClass)

import Html exposing (Attribute)
import Html.Attributes exposing (classList)


formFieldTransitionClass : String
formFieldTransitionClass =
    "duration-200 ease-in-out"


formFieldClass : { r | validation : Result error (), disabled : Bool } -> Attribute msg
formFieldClass { validation, disabled } =
    classList
        [ ( "group flex border-2 rounded-lg leading-none transition-all", True )
        , ( formFieldTransitionClass, True )
        , ( case ( validation, disabled ) of
                ( _, True ) ->
                    "bg-neutral-100 border-neutral-200 placeholder:text-neutral-400"

                ( Ok (), _ ) ->
                    "focus-within:border-cyan-600 hover:border-cyan-600 focus-within:ring ring-cyan-200 text-gray-900"

                ( Err _, _ ) ->
                    "border-red-500  focus-within:ring ring-red-200 text-red-800 placeholder:text-red-200"
          , True
          )
        ]
