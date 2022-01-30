module Page.TextArea exposing (view)

import Components.Label as Label
import Components.TextArea as TextArea
import Section exposing (Section)


view : List (Section msg)
view =
    [ Section.section "Default"
        [ TextArea.view
            [ TextArea.placeholder "Input Text"
            ]
        , TextArea.view
            [ TextArea.value "Input Text"
            ]
        , TextArea.view
            [ TextArea.value "Input Text"
            , TextArea.validation (Err "Error message")
            ]
        , TextArea.view
            [ TextArea.placeholder "Input Text"
            , TextArea.disabled True
            ]
        , TextArea.view
            [ TextArea.placeholder "Input Text"
            , TextArea.label Label.horizontal (Label.single "Label")
            ]
        , TextArea.view
            [ TextArea.placeholder "Input Text"
            , TextArea.label Label.horizontal (Label.double "Label" "Second label")
            ]
        , TextArea.view
            [ TextArea.placeholder "Input Text"
            , TextArea.label Label.vertical (Label.single "Label")
            ]
        , TextArea.view
            [ TextArea.placeholder "Input Text"
            , TextArea.label Label.vertical (Label.double "Label" "Second label")
            ]
        ]
    ]
