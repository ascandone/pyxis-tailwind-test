module Utils exposing (concatArgs, getMakeConfig)


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
