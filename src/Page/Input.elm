module Page.Input exposing (view)

import Components.Input as Input
import Components.Label as Label
import FeatherIcons
import Section exposing (Section)


view : List (Section msg)
view =
    [ Section.section "Default"
        [ Input.view
            [ Input.placeholder "Input Text"
            ]
        , Input.view
            [ Input.value "Input Text"
            ]
        , Input.view
            [ Input.validation (Err "Error message")
            , Input.placeholder "Input Text"
            ]
        , Input.view
            [ Input.disabled True
            , Input.placeholder "Input Text"
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.size Input.small
            ]
        ]
    , Section.section "Labeled"
        [ Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.vertical (Label.single "Label")
            , Input.id "item-id"
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.vertical (Label.double "Label" "Second label")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.horizontal (Label.single "Label")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.horizontal (Label.double "Label" "Second label")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.vertical (Label.single "Label")
            , Input.size Input.small
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.vertical (Label.double "Label" "Second label")
            , Input.size Input.small
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.horizontal (Label.single "Label")
            , Input.size Input.small
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.label Label.horizontal (Label.double "Label" "Second label")
            , Input.size Input.small
            ]
        ]
    , Section.section "Text addon"
        [ Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.leading (Input.textAddon "€")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.trailing (Input.textAddon "€")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.validation (Err "Error message")
            , Input.addon Input.leading (Input.textAddon "€")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.disabled True
            , Input.addon Input.leading (Input.textAddon "€")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.size Input.small
            , Input.addon Input.leading (Input.textAddon "€")
            ]
        ]
    , Section.section "Icon addon"
        [ Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.leading (Input.iconAddon FeatherIcons.link)
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.trailing (Input.iconAddon FeatherIcons.link)
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.leading (Input.iconAddon FeatherIcons.link)
            , Input.validation (Err "Error message")
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.leading (Input.iconAddon FeatherIcons.link)
            , Input.disabled True
            ]
        , Input.view
            [ Input.placeholder "Input Text"
            , Input.addon Input.leading (Input.iconAddon FeatherIcons.link)
            , Input.size Input.small
            ]
        ]
    ]
