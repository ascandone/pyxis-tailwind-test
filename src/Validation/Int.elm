module Validation.Int exposing
    ( max
    , min
    )

import Validation exposing (Validation)


min : Int -> String -> Validation Int Int
min min_ =
    Validation.fromPredicate (\n -> n >= min_)


max : Int -> String -> Validation Int Int
max max_ =
    Validation.fromPredicate (\n -> n <= max_)
