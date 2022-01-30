module Page.TextField exposing (view)

import Components.Label as Label
import Components.TextField as TextField
import FeatherIcons
import Section exposing (Section)


view : List (Section msg)
view =
    [ Section.section "Default"
        [ TextField.view
            [ TextField.placeholder "Input Text"
            ]
        , TextField.view
            [ TextField.value "Input Text"
            ]
        , TextField.view
            [ TextField.validation (Err "Error message")
            , TextField.placeholder "Input Text"
            ]
        , TextField.view
            [ TextField.disabled True
            , TextField.placeholder "Input Text"
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.size TextField.small
            ]
        ]
    , Section.section "Labeled"
        [ TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.vertical (Label.single "Label")
            , TextField.id "item-id"
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.vertical (Label.double "Label" "Second label")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.horizontal (Label.single "Label")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.horizontal (Label.double "Label" "Second label")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.vertical (Label.single "Label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.vertical (Label.double "Label" "Second label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.horizontal (Label.single "Label")
            , TextField.size TextField.small
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.label Label.horizontal (Label.double "Label" "Second label")
            , TextField.size TextField.small
            ]
        ]
    , Section.section "Text addon"
        [ TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.trailing (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.validation (Err "Error message")
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.disabled True
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.size TextField.small
            , TextField.addon TextField.leading (TextField.textAddon "€")
            ]
        ]
    , Section.section "Icon addon"
        [ TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.trailing (TextField.iconAddon FeatherIcons.link)
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.validation (Err "Error message")
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.disabled True
            ]
        , TextField.view
            [ TextField.placeholder "Input Text"
            , TextField.addon TextField.leading (TextField.iconAddon FeatherIcons.link)
            , TextField.size TextField.small
            ]
        ]
    ]
