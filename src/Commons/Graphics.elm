module Commons.Graphics exposing (arrowLine, calculateViewBoxSize, draggableCircle, draggableDoubleStrokedCircle, noSelectableText, personIcon)

import Commons.Constant exposing (const_VIEWBOX_PADDING)
import Commons.Mouse exposing (onMouseDown)
import Commons.Msg exposing (Msg)
import Commons.Position exposing (NodePositions, Position, const_POSITION_ONE, const_POSITION_ZERO, getNodePosition, stringifyXY)
import Css.Class exposing (noSelectSvgAttributes)
import Diagrams.Type exposing (Node, NodeId)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- LINE


arrowLine : NodeId -> Float -> NodePositions -> Node -> Float -> List (Svg msg)
arrowLine fromNodeId fromRadius positions toNode toRadius =
    let
        fromPosition =
            getNodePosition fromNodeId positions

        toPosition =
            Dict.get toNode.name positions |> Maybe.withDefault const_POSITION_ONE

        ( start, end ) =
            calculateArrowPoints fromPosition toPosition fromRadius toRadius

        ( startX, startY ) =
            stringifyXY start

        ( endX, endY ) =
            stringifyXY end
    in
    [ line
        [ x1 startX
        , y1 startY
        , x2 endX
        , y2 endY
        , stroke "black"
        , strokeWidth "2"
        , markerEnd "url(#arrow)"
        ]
        []
    ]



-- TEXT


noSelectableText : NodeId -> Position -> Svg Msg
noSelectableText nodeId position =
    let
        ( posX, posY ) =
            stringifyXY position
    in
    text_
        ([ x posX, y posY, textAnchor "middle", dy ".3em" ]
            ++ noSelectSvgAttributes
        )
        [ Svg.text nodeId ]



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
        ( posX, posY ) =
            stringifyXY position

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



-- PERSON


personIcon : NodeId -> Position -> Svg Msg
personIcon nodeId position =
    let
        ( posX, posY ) =
            stringifyXY position
    in
    g [ onMouseDown nodeId ]
        [ circle [ cx posX, cy posY, r "10", fill "black" ] [] -- head
        , line [ x1 posX, y1 (String.fromFloat (position.y + 10)), x2 posX, y2 (String.fromFloat (position.y + 30)), stroke "black", strokeWidth "2" ] [] -- body
        , line [ x1 (String.fromFloat (position.x - 10)), y1 (String.fromFloat (position.y + 15)), x2 (String.fromFloat (position.x + 10)), y2 (String.fromFloat (position.y + 15)), stroke "black", strokeWidth "2" ] [] -- arms
        , line [ x1 (String.fromFloat position.x), y1 (String.fromFloat (position.y + 30)), x2 (String.fromFloat (position.x + 10)), y2 (String.fromFloat (position.y + 40)), stroke "black", strokeWidth "2" ] [] -- leg1
        , line [ x1 (String.fromFloat position.x), y1 (String.fromFloat (position.y + 30)), x2 (String.fromFloat (position.x - 10)), y2 (String.fromFloat (position.y + 40)), stroke "black", strokeWidth "2" ] [] -- leg2
        ]



-- CALCULATION


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
