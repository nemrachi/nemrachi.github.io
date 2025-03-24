module Common.Mouse exposing (onMouseDown, positionDecoder)

import Common.Msg exposing (Msg(..))
import Common.Position exposing (Position)
import Diagram.Type exposing (NodeId)
import Json.Decode as D
import Svg
import Svg.Events as E


onMouseDown : NodeId -> Svg.Attribute Msg
onMouseDown nodeId =
    E.on "mousedown" (D.map (DragStart nodeId) positionDecoder)


positionDecoder : D.Decoder Position
positionDecoder =
    D.map2 Position
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)
