module Maybe.Extra exposing (andThen, mapToList)


mapToList : (a -> b) -> Maybe a -> List b
mapToList f m =
    case m of
        Nothing ->
            []

        Just x ->
            [ f x ]


andThen : Maybe a -> Maybe (a -> b) -> Maybe b
andThen =
    Maybe.map2 (|>)
