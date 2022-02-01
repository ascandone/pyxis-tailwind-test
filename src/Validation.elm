module Validation exposing
    ( Validation
    , fromMaybe
    , fromPredicate
    , map
    )


type alias Validation from to =
    from -> Result String to


fromPredicate : (a -> Bool) -> String -> Validation a a
fromPredicate pred reason x =
    if pred x then
        Ok x

    else
        Err reason


fromMaybe : String -> Validation (Maybe a) a
fromMaybe reason mX =
    case mX of
        Nothing ->
            Err reason

        Just x ->
            Ok x


map : (a -> b) -> Validation from a -> Validation from b
map f validation from =
    Result.map f (validation from)
