module Page.Button exposing (view)

import Components.Button as Button
import FeatherIcons
import Html
import Html.Attributes
import Section exposing (Section)


view : List (Section msg)
view =
    let
        btnGroup =
            Html.div [ Html.Attributes.class "flex gap-x-8 gap-y-4 items-center flex-wrap" ]

        makeGroup constructor attrs =
            btnGroup
                [ constructor (Button.size Button.small :: attrs) "Text"
                , constructor (Button.size Button.medium :: attrs) "Text"
                , constructor (Button.size Button.large :: attrs) "Text"
                , constructor (Button.size Button.huge :: attrs) "Text"
                ]

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
                ]
    in
    [ makeSection Button.primary "Primary"
    , makeSection Button.secondary "Secondary"
    , makeSection Button.tertiary "Tertiary"
    , makeSection Button.brand "Brand"
    , makeSection Button.ghost "Ghost"
    ]
