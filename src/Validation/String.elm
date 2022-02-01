module Validation.String exposing (notEmpty)

import Validation exposing (Validation)


notEmpty : String -> Validation String String
notEmpty =
    Validation.fromPredicate (not << String.isEmpty)
