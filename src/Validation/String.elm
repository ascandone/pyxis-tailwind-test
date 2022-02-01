module Validation.String exposing (notEmpty)

import Validation exposing (Validation)


notEmpty : String -> Validation String String
notEmpty reason =
    Validation.fromPredicate
        (not << String.isEmpty)
        reason
