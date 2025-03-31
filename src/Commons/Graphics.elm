module Commons.Graphics exposing (calculateArrowPoints, calculateViewBoxSize, draggableCircle, draggableDoubleStrokedCircle)

import Commons.Constant exposing (const_VIEWBOX_PADDING)
import Commons.Mouse exposing (onMouseDown)
import Commons.Msg exposing (Msg)
import Commons.Position exposing (NodePositions, Position, const_POSITION_ZERO)
import Diagrams.Type exposing (NodeId)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- CIRCLE


draggableCircle : NodeId -> Position -> Float -> String -> Svg Msg
draggableCircle nodeId position radius color =
    circleWithAttrs position radius color [ onMouseDown nodeId ]


draggableDoubleStrokedCircle : NodeId -> Position -> Float -> String -> String -> Svg Msg
draggableDoubleStrokedCircle nodeId position radius innerColor outerColor =
    Svg.g []
        [ circleWithAttrs position radius outerColor [ stroke innerColor, strokeWidth "1" ]
        , circleWithAttrs position (radius - 5) innerColor [ onMouseDown nodeId ]
        ]


circleWithAttrs : Position -> Float -> String -> List (Attribute Msg) -> Svg Msg
circleWithAttrs position radius color extraAttrs =
    let
        posX =
            String.fromFloat position.x

        posY =
            String.fromFloat position.y

        rad =
            String.fromFloat radius
    in
    circle
        ([ cx posX
         , cy posY
         , r rad
         , fill color
         ]
            ++ extraAttrs
        )
        []



-- CALCULATION


calculateArrowPoints : Position -> Position -> Float -> Float -> ( Position, Position )
calculateArrowPoints parent child parentRadius childRadius =
    let
        dx =
            child.x - parent.x

        dy =
            child.y - parent.y

        distance =
            sqrt (dx * dx + dy * dy)

        offsetXParent =
            (dx / distance) * parentRadius

        offsetYParent =
            (dy / distance) * parentRadius

        offsetXChild =
            (dx / distance) * childRadius

        offsetYChild =
            (dy / distance) * childRadius
    in
    ( { x = parent.x + offsetXParent, y = parent.y + offsetYParent }
    , { x = child.x - offsetXChild, y = child.y - offsetYChild }
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



-- HELPERS


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
