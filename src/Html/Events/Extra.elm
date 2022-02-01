module Html.Events.Extra exposing (KeydownEvent, onKeyDown)

import Html
import Html.Events
import Json.Decode as Decode


type alias KeydownEvent =
    { keyCode : Int
    }


decodeKeyDownEvent : (KeydownEvent -> msg) -> Decode.Decoder msg
decodeKeyDownEvent tagger =
    Html.Events.keyCode
        |> Decode.map KeydownEvent
        |> Decode.map tagger


onKeyDown : (KeydownEvent -> msg) -> Html.Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (decodeKeyDownEvent tagger)
