module Maybe.Extra exposing (andMap, mapToList)


mapToList : (a -> b) -> Maybe a -> List b
mapToList f m =
    case m of
        Nothing ->
            []

        Just x ->
            [ f x ]


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)
