module Validation.String exposing
    ( notEmpty
    , optional
    , toInt
    , trim
    )

import Validation exposing (Validation)


notEmpty : String -> Validation String String
notEmpty =
    Validation.fromPredicate (not << String.isEmpty)


trim : Validation String String -> Validation String String
trim =
    Validation.map String.trim


optional : Validation String to -> Validation String (Maybe to)
optional validation str =
    if String.isEmpty str then
        Ok Nothing

    else
        Result.map Just (validation str)


toInt : String -> Validation String Int
toInt reason raw =
    case String.toInt raw of
        Nothing ->
            Err reason

        Just n ->
            Ok n
