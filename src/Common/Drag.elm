module Common.Drag exposing (Drag, applyDragToPosition)

import Common.Position exposing (NodePositions, Position)
import Diagram.Type exposing (NodeId)
import Dict


type alias Drag =
    { start : Position
    , current : Position
    , nodeId : NodeId
    }


applyDragToPosition : Maybe Drag -> NodePositions -> NodePositions
applyDragToPosition maybeDrag positions =
    case maybeDrag of
        Just { start, current, nodeId } ->
            Dict.update nodeId
                (Maybe.map
                    (\pos ->
                        { x = pos.x + (start.x - current.x)
                        , y = pos.y + (start.y - current.y)
                        }
                    )
                )
                positions

        Nothing ->
            positions
