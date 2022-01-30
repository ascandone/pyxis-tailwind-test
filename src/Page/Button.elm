module Page.Button exposing (view)

import Components.Button as Button
import Html
import Html.Attributes
import Section exposing (Section)


view : List (Section msg)
view =
    let
        btnGroup =
            Html.div [ Html.Attributes.class "flex gap-x-8 gap-y-3 items-start flex-wrap" ]
    in
    [ Section.section "Primary"
        [ btnGroup
            [ Button.primary [ Button.size Button.small ] "Text"
            , Button.primary [ Button.size Button.medium ] "Text"
            , Button.primary [ Button.size Button.large ] "Text"
            , Button.primary [ Button.size Button.huge ] "Text"
            , Button.primary [ Button.loading True ] "Text"
            ]
        , btnGroup
            [ Button.primary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Secondary"
        [ btnGroup
            [ Button.secondary [ Button.size Button.small ] "Text"
            , Button.secondary [ Button.size Button.medium ] "Text"
            , Button.secondary [ Button.size Button.large ] "Text"
            , Button.secondary [ Button.size Button.huge ] "Text"
            ]
        , btnGroup
            [ Button.secondary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Tertiary"
        [ btnGroup
            [ Button.tertiary [ Button.size Button.small ] "Text"
            , Button.tertiary [ Button.size Button.medium ] "Text"
            , Button.tertiary [ Button.size Button.large ] "Text"
            , Button.tertiary [ Button.size Button.huge ] "Text"
            , Button.tertiary [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Brand"
        [ btnGroup
            [ Button.brand [ Button.size Button.small ] "Text"
            , Button.brand [ Button.size Button.medium ] "Text"
            , Button.brand [ Button.size Button.large ] "Text"
            , Button.brand [ Button.size Button.huge ] "Text"
            , Button.brand [ Button.loading True ] "Text"
            ]
        ]
    , Section.section "Ghost"
        [ btnGroup
            [ Button.ghost [ Button.size Button.small ] "Text"
            , Button.ghost [ Button.size Button.medium ] "Text"
            , Button.ghost [ Button.size Button.large ] "Text"
            , Button.ghost [ Button.size Button.huge ] "Text"
            ]
        ]
    , Section.section "Loading"
        [ btnGroup
            [ Button.primary [ Button.loading True, Button.size Button.small ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.medium ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.large ] "Text"
            , Button.primary [ Button.loading True, Button.size Button.huge ] "Text"
            ]
        ]
    ]
