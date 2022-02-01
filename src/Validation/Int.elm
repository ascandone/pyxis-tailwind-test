module Validation.Int exposing
    ( fromString
    , max
    , min
    )

import Validation exposing (Validation)


fromString : String -> Validation String Int
fromString reason raw =
    case String.toInt raw of
        Nothing ->
            Err reason

        Just n ->
            Ok n


min : Int -> String -> Validation Int Int
min min_ =
    Validation.fromPredicate (\n -> n >= min_)


max : Int -> String -> Validation Int Int
max max_ =
    Validation.fromPredicate (\n -> n <= max_)
