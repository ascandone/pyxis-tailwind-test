module Utils exposing (concatArgs, getMakeConfig, stateClass)

import Html exposing (Attribute)
import Html.Attributes


getMakeConfig :
    { unwrap : attribute -> config -> config
    , defaultConfig : config
    }
    -> List attribute
    -> config
getMakeConfig args =
    List.foldl (\attr conf -> args.unwrap attr conf) args.defaultConfig


concatArgs : (List a -> b) -> List (List a) -> b
concatArgs f args =
    f (List.concat args)


stateClass :
    Result err value
    -> Bool
    ->
        { disabled : String
        , error : String
        , default : String
        }
    -> Attribute msg
stateClass validation disabled cls =
    Html.Attributes.class <|
        case ( validation, disabled ) of
            ( _, True ) ->
                cls.disabled

            ( Err _, _ ) ->
                cls.error

            ( Ok _, _ ) ->
                cls.default
