module FormState exposing (FormState(..), blur, focus, input)

{-| -}


{-| Untouched := Not focused yet
Touched := trigghered `focus` at least once
-}
type FormState
    = Untouched -- Initial state
    | Touched
        { blurredAtLeastOnce : Bool
        , focused : Bool -- currently focused
        , dirty : Bool -- edited text at least once
        }


focus : FormState -> FormState
focus state =
    case state of
        Untouched ->
            Touched
                { blurredAtLeastOnce = False
                , focused = True
                , dirty = False
                }

        Touched data ->
            Touched
                { data | focused = True }


blur : FormState -> FormState
blur state =
    case state of
        Touched data ->
            Touched
                { data | focused = False, blurredAtLeastOnce = True }

        -- Should not happen
        _ ->
            state


input : FormState -> FormState
input state =
    case state of
        Touched data ->
            Touched { data | dirty = True }

        -- Should not happen
        _ ->
            state
