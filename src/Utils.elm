module Utils exposing (concatArgs, getMakeConfig, stateClass)


getMakeConfig :
    { unwrap : attribute -> config -> config
    , defaultConfig : config
    }
    -> List attribute
    -> config
getMakeConfig args =
    List.foldr (\attr conf -> args.unwrap attr conf) args.defaultConfig


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
    -> String
stateClass validation disabled cls =
    case ( validation, disabled ) of
        ( _, True ) ->
            cls.disabled

        ( Err _, _ ) ->
            cls.error

        ( Ok _, _ ) ->
            cls.default
