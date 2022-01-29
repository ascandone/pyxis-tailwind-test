module Components.Internal exposing (formFieldClass)

import Html exposing (Attribute)
import Html.Attributes exposing (classList)


formFieldClass : { r | validation : Result error (), disabled : Bool } -> Attribute msg
formFieldClass { validation, disabled } =
    classList
        [ ( "border-2 rounded-lg leading-none transition-all duration-200 ease-in-out", True )
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
