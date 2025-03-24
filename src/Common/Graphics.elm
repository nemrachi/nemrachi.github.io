module Common.Graphics exposing (calculateArrowPoints, calculateViewBoxSize)

import Common.Constant exposing (const_NODE_RADIUS, const_VIEWBOX_PADDING)
import Common.Position exposing (NodePositions, Position, const_POSITION_ZERO)
import Dict


calculateArrowPoints : Position -> Position -> ( Position, Position )
calculateArrowPoints parent child =
    let
        dx =
            child.x - parent.x

        dy =
            child.y - parent.y

        distance =
            sqrt (dx * dx + dy * dy)

        offsetX =
            (dx / distance) * const_NODE_RADIUS

        offsetY =
            (dy / distance) * const_NODE_RADIUS
    in
    ( { x = parent.x + offsetX, y = parent.y + offsetY }
    , { x = child.x - offsetX, y = child.y - offsetY }
    )


calculateViewBoxSize : NodePositions -> String
calculateViewBoxSize positions =
    let
        ( minPos, maxPos ) =
            calculateMinMax positions

        minX =
            minPos.x - const_VIEWBOX_PADDING

        minY =
            minPos.y - const_VIEWBOX_PADDING

        maxX =
            maxPos.x + const_VIEWBOX_PADDING

        maxY =
            maxPos.y + const_VIEWBOX_PADDING
    in
    String.join " "
        [ String.fromFloat minX
        , String.fromFloat minY
        , String.fromFloat (maxX - minX)
        , String.fromFloat (maxY - minY)
        ]


calculateMinMax : NodePositions -> ( Position, Position )
calculateMinMax positions =
    Dict.values positions
        |> List.foldl
            (\pos ( minPos, maxPos ) ->
                ( { x = Basics.min minPos.x pos.x, y = Basics.min minPos.y pos.y }
                , { x = Basics.max maxPos.x pos.x, y = Basics.max maxPos.y pos.y }
                )
            )
            ( const_POSITION_ZERO, const_POSITION_ZERO )
