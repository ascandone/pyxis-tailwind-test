module Components.Label exposing
    ( Position
    , Type
    , double
    , horizontal
    , single
    , vertical
    )

import Components.Label.Internal as Internal


type alias Position =
    Internal.Position


vertical : Position
vertical =
    Internal.Vertical


horizontal : Position
horizontal =
    Internal.Horizontal


type alias Type =
    Internal.Type


single : String -> Type
single =
    Internal.Single


double : String -> String -> Type
double =
    Internal.Double
