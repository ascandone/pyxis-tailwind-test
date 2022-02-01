module FormParser exposing
    ( Field
    , hardcoded
    , input
    , required
    , succeed
    )

import Components.InputValidation as InputValidation


type alias Field field model data =
    (model -> Maybe (field -> data)) -> model -> Maybe data


succeed : value -> model -> Maybe value
succeed value _ =
    Just value


required : (model -> Maybe field) -> Field field model data
required getFieldData f model =
    Maybe.map2 (<|) (f model) (getFieldData model)


hardcoded : field -> Field field model data
hardcoded field =
    required (\_ -> Just field)


input : (model -> InputValidation.Model field) -> Field field model data
input getter =
    required (InputValidation.getData << getter)
