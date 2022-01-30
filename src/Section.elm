module Section exposing
    ( Section
    , section
    , view
    )

import Html exposing (Html)
import Html.Attributes exposing (class)


type Section msg
    = Section String (List (Html msg))


section : String -> List (Html msg) -> Section msg
section =
    Section


viewSection : Section msg -> Html msg
viewSection (Section header_ children) =
    Html.section [ class "bg-white rounded-md shadow-soft px-8 pt-4 pb-10" ]
        [ Html.h3 [ class "text-2xl tracking-wide text-gray-900 font-serif" ] [ Html.text header_ ]
        , Html.div [ class "h-2" ] []
        , Html.div [ class "space-y-8" ] children
        ]


view : List (Section msg) -> Html msg
view sections =
    Html.div [ class "antialiased h-full space-y-12 mx-auto max-w-screen-lg h-full" ]
        (List.map viewSection sections)
