module Commons.Drag exposing (Drag, applyDragToPosition, preserveDraggedPositions)

import Commons.Position exposing (NodePositions, Position)
import Diagrams.Type exposing (NodeId)
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
                        { x = pos.x + (current.x - start.x)
                        , y = pos.y + (current.y - start.y)
                        }
                    )
                )
                positions

        Nothing ->
            positions


preserveDraggedPositions : NodePositions -> NodePositions -> NodePositions
preserveDraggedPositions oldPositions newPositions =
    Dict.merge
        -- if only in oldPositions, skip
        (\_ _ acc -> acc)
        -- if in both, use value from oldPositions
        (\key oldPos _ acc -> Dict.insert key oldPos acc)
        -- if only in newPositions, keep original value from newPositions
        (\key newPos acc -> Dict.insert key newPos acc)
        oldPositions
        newPositions
        Dict.empty
