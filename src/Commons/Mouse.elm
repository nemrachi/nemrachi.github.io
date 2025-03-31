module Commons.Mouse exposing (onMouseDown, onMouseMove, onMouseUp)

import Browser.Events as BE
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (Position)
import Diagrams.Type exposing (NodeId)
import Json.Decode as D
import Svg
import Svg.Events as SE


onMouseDown : NodeId -> Svg.Attribute Msg
onMouseDown nodeId =
    SE.on "mousedown" (D.map (DragStart nodeId) positionDecoder)


onMouseMove : Sub Msg
onMouseMove =
    BE.onMouseMove (D.map DragAt positionDecoder)


onMouseUp : Sub Msg
onMouseUp =
    BE.onMouseUp (D.succeed DragEnd)


positionDecoder : D.Decoder Position
positionDecoder =
    D.map2 Position
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)
