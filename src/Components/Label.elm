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


type alias Type =
    Internal.Type


single : String -> Type
single =
    Internal.Single


double : String -> String -> Type
double =
    Internal.Double


horizontal : Position
horizontal =
    Internal.horizontal


vertical : Position
vertical =
    Internal.vertical
