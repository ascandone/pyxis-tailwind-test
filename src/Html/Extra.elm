module Html.Extra exposing (viewMaybe)

import Html exposing (Html)


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f m =
    case m of
        Nothing ->
            Html.text ""

        Just x ->
            f x
