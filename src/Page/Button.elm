module Page.Button exposing (view)

import Components.Button as Button
import FeatherIcons
import Html
import Html.Attributes
import Section exposing (Section)


makeGroup : (List (Button.Attribute msg) -> String -> Html.Html msg) -> List (Button.Attribute msg) -> Html.Html msg
makeGroup constructor attrs =
    Html.div [ Html.Attributes.class "flex gap-x-8 gap-y-4 items-center flex-wrap" ]
        [ constructor (Button.size Button.small :: attrs) "Text"
        , constructor (Button.size Button.medium :: attrs) "Text"
        , constructor (Button.size Button.large :: attrs) "Text"
        , constructor (Button.size Button.huge :: attrs) "Text"
        ]


makeSection : (List (Button.Attribute msg) -> String -> Html.Html msg) -> String -> Section msg
makeSection constructor header =
    let
        makeGroup_ =
            makeGroup constructor
    in
    Section.section header
        [ makeGroup_ []
        , makeGroup_ [ Button.contentWidth True ]
        , makeGroup_ [ Button.loading True ]
        , makeGroup_ [ Button.icon Button.leadingPlacement FeatherIcons.edit2 ]
        , makeGroup_ [ Button.icon Button.trailingPlacement FeatherIcons.edit2 ]
        , makeGroup_ [ Button.icon Button.only FeatherIcons.edit2 ]
        , makeGroup_ [ Button.shadow True ]
        , makeGroup_ [ Button.disabled True, Button.icon Button.leadingPlacement FeatherIcons.edit2 ]
        ]


view : List (Section msg)
view =
    [ makeSection Button.primary "Primary"
    , makeSection Button.secondary "Secondary"
    , makeSection Button.tertiary "Tertiary"
    , makeSection Button.brand "Brand"
    , makeSection Button.ghost "Ghost"
    ]
