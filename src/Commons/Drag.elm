module Commons.Drag exposing (Drag, applyDragToPosition, onMouseDown, onMouseMove, onMouseUp)

import Browser.Events as BE
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (MousePosition, NodePositions, Position)
import Diagrams.Graph exposing (NodeId)
import Dict
import Json.Decode as D
import Svg
import Svg.Events as SE


type alias Drag =
    { start : MousePosition
    , current : MousePosition
    , nodeId : NodeId
    , originalNodePosition : Position
    }


applyDragToPosition : Maybe Drag -> NodePositions -> NodePositions
applyDragToPosition maybeDrag positions =
    case maybeDrag of
        Just { start, current, nodeId, originalNodePosition } ->
            Dict.update nodeId
                (\_ ->
                    Just
                        { x = originalNodePosition.x + (current.x - start.x)
                        , y = originalNodePosition.y + (current.y - start.y)
                        }
                )
                positions

        Nothing ->
            positions



-- MOUSE EVENTS


onMouseDown : NodeId -> Position -> Svg.Attribute Msg
onMouseDown nodeId nodeOriginalPosition =
    SE.on "mousedown" (D.map (DragStart nodeId nodeOriginalPosition) positionDecoder)


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
