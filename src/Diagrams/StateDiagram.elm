module Diagrams.StateDiagram exposing (parseStateDiagram, renderStateDiagram)

import Commons.Constant exposing (const_END, const_NODE_BOX_CORNER_RADIUS, const_SMALL_NODE_BOX_SIZE, const_START, const_START_END_NODE_RADIUS, const_SVG_ARROW)
import Commons.Drag exposing (preserveDraggedPositions)
import Commons.Graphics as Graphics
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ZERO)
import Commons.TextParser exposing (parseEdgeLabel, parsePoint)
import Diagrams.Type exposing (Diagram, Edge, Node, NodeId, NodeSize)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- TODO
-- Observer - MVU
-- Factory - creating objects without exposing the instantiation logic (positionZero...)
-- Builder - construct complex objects step-by-step
-- Strategy - Funkcia spusti funkciu A alebo B podla niecoho
-- Composite - treat individual objects and compositions of objects uniformly (??)
-- PARSING
-- TODO https://sporto.github.io/elm-patterns/advanced/railway.html


parseStateDiagram : List String -> NodePositions -> ( Diagram, NodePositions )
parseStateDiagram diagramLines nodePositions =
    diagramLines
        -- https://sporto.github.io/elm-patterns/basic/unwrap-maybe-early.html
        |> List.filterMap parseLine
        |> buildDiagram
        |> (\diagram -> ( diagram, calculatePositions const_START const_POSITION_ZERO diagram Dict.empty ))
        |> (\( diagram, generatedPositions ) -> ( diagram, preserveDraggedPositions nodePositions generatedPositions ))



-- GET EDGES


parseLine : String -> Maybe Edge
parseLine line =
    case String.words line of
        from :: "-->" :: to :: label ->
            Just
                { from = parsePoint from const_START
                , to = parsePoint to const_END
                , label = parseEdgeLabel label
                }

        _ ->
            Nothing



-- GET DIAGRAM


buildDiagram : List Edge -> Diagram
buildDiagram edges =
    List.foldl addEdgeToDiagram Dict.empty edges


addEdgeToDiagram : Edge -> Diagram -> Diagram
addEdgeToDiagram edge diagram =
    let
        parent =
            edge.from

        childNode =
            { name = edge.to, edgeLabel = edge.label }
    in
    Dict.update parent
        (\maybeNodes ->
            Just (childNode :: Maybe.withDefault [] maybeNodes)
        )
        diagram



-- RENDERING
-- TODO: render labels with https://sporto.github.io/elm-patterns/basic/conditional-rendering.html


renderStateDiagram : Diagram -> NodePositions -> Svg Msg
renderStateDiagram diaram positions =
    let
        vb =
            Graphics.calculateViewBoxSize positions
    in
    svg
        [ viewBox vb
        , preserveAspectRatio "xMidYMid meet"
        , width "100%"
        , height "100%"
        ]
        (const_SVG_ARROW
            ++ List.concatMap
                (\( parent, children ) ->
                    List.concatMap (renderTransition parent positions) children
                )
                (Dict.toList diaram)
            ++ List.concatMap
                (\( node, position ) ->
                    renderNode node position
                )
                (Dict.toList positions)
        )



-- TODO https://sporto.github.io/elm-patterns/basic/arguments-list.html


renderTransition : NodeId -> NodePositions -> Node -> List (Svg msg)
renderTransition parentId positions child =
    Graphics.arrowLine parentId (getNodeSize parentId) positions child (getNodeSize child.name)


getNodeSize : NodeId -> NodeSize
getNodeSize nodeId =
    if nodeId == const_START || nodeId == const_END then
        const_SMALL_NODE_BOX_SIZE

    else
        Graphics.calculateNodeSize nodeId


renderNode : NodeId -> Position -> List (Svg Msg)
renderNode nodeId position =
    if nodeId == const_START then
        [ Graphics.draggableCircle nodeId position const_START_END_NODE_RADIUS "black" ]

    else if nodeId == const_END then
        [ Graphics.draggableDoubleStrokedCircle nodeId position const_START_END_NODE_RADIUS "black" "white" ]

    else
        [ Graphics.draggableRoundedBoxWithText nodeId position const_NODE_BOX_CORNER_RADIUS "lightgreen" ]



-- [ Graphics.draggableCircle nodeId position const_NODE_RADIUS "lightgreen"
-- , Graphics.noSelectableText nodeId position
-- ]
