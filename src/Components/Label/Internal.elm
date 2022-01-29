module Components.Label.Internal exposing
    ( Label
    , Position(..)
    , Type(..)
    )


type alias Label =
    { position : Position
    , label : String
    , secondaryLabel : Maybe String
    }


type Position
    = Vertical
    | Horizontal


type Type
    = Single String
    | Double String String
