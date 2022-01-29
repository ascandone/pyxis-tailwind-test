module Maybe.Extra exposing (mapToList)


mapToList : (a -> b) -> Maybe a -> List b
mapToList f m =
    case m of
        Nothing ->
            []

        Just x ->
            [ f x ]
