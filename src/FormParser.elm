module FormParser exposing
    ( Field
    , hardcoded
    , input
    , required
    , succeed
    )

import Components.InputValidation as InputValidation


type alias Field fieldData model data =
    (model -> Maybe (fieldData -> data)) -> model -> Maybe data


succeed : value -> model -> Maybe value
succeed value _ =
    Just value


required : (model -> Maybe fieldData) -> Field fieldData model data
required getFieldData f model =
    Maybe.map2 (<|) (f model) (getFieldData model)


hardcoded : fieldData -> Field fieldData model data
hardcoded fieldData =
    required (\_ -> Just fieldData)


input : (model -> InputValidation.Model fieldData) -> Field fieldData model data
input getter =
    required (InputValidation.getData << getter)
