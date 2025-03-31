module Diagrams.StateDiagram exposing (parseStateDiagram, renderStateDiagram)

import Commons.Constant exposing (const_END, const_NODE_RADIUS, const_START, const_START_END_NODE_RADIUS, const_SVG_ARROW)
import Commons.Graphics exposing (calculateArrowPoints, calculateViewBoxSize)
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ONE, const_POSITION_ZERO, getNodePosition)
import Commons.TextParser exposing (parseEdgeLabel, parsePoint)
import Diagrams.Type exposing (Diagram, Edge, Node, NodeId)
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
        |> List.filterMap parseLine
        -- TODO https://sporto.github.io/elm-patterns/basic/unwrap-maybe-early.html
        |> buildDiagram
        |> (\diagram -> ( diagram, calculatePositions const_START const_POSITION_ZERO diagram Dict.empty ))
        |> (\( diagram, generatedPositions ) -> ( diagram, updateNodePositions nodePositions generatedPositions ))



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



-- UPDATE NODE POSITIONS


updateNodePositions : NodePositions -> NodePositions -> NodePositions
updateNodePositions oldPositions newPositions =
    Dict.merge
        -- if only in oldPositions, skip
        (\_ _ acc -> acc)
        -- if key is in both, use value from oldPositions
        (\key oldPos _ acc -> Dict.insert key oldPos acc)
        -- if only in newPositions, keep original value from newPositions
        (\key newPos acc -> Dict.insert key newPos acc)
        oldPositions
        newPositions
        Dict.empty



-- RENDERING
-- TODO https://sporto.github.io/elm-patterns/basic/conditional-rendering.html
-- TODO maybe https://sporto.github.io/elm-patterns/architecture/reusable-views.html


renderStateDiagram : Diagram -> NodePositions -> Svg Msg
renderStateDiagram diaram positions =
    let
        vb =
            calculateViewBoxSize positions
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
            ++ List.concatMap (\( node, position ) -> renderNode node position) (Dict.toList positions)
        )


renderTransition : NodeId -> NodePositions -> Node -> List (Svg msg)
renderTransition parentId positions child =
    let
        parentPosition =
            getNodePosition parentId positions

        childPosition =
            Dict.get child.name positions |> Maybe.withDefault const_POSITION_ONE

        parentRadius =
            getNodeRadius parentId

        childRadius =
            getNodeRadius child.name

        ( start, end ) =
            calculateArrowPoints parentPosition childPosition parentRadius childRadius
    in
    [ line
        [ x1 (String.fromFloat start.x)
        , y1 (String.fromFloat start.y)
        , x2 (String.fromFloat end.x)
        , y2 (String.fromFloat end.y)
        , stroke "black"
        , strokeWidth "2"
        , markerEnd "url(#arrow)"
        ]
        []
    ]


getNodeRadius : NodeId -> Float
getNodeRadius nodeId =
    case nodeId of
        "⒮" ->
            const_START_END_NODE_RADIUS

        "⒠" ->
            const_START_END_NODE_RADIUS

        _ ->
            const_NODE_RADIUS



-- TODO https://sporto.github.io/elm-patterns/basic/arguments-list.html


renderNode : NodeId -> Position -> List (Svg Msg)
renderNode nodeId position =
    case nodeId of
        "⒮" ->
            [ Commons.Graphics.draggableCircle nodeId position const_START_END_NODE_RADIUS "black" ]

        "⒠" ->
            [ Commons.Graphics.draggableDoubleStrokedCircle nodeId position const_START_END_NODE_RADIUS "black" "white" ]

        _ ->
            [ Commons.Graphics.draggableCircle nodeId position const_NODE_RADIUS "lightgreen"
            , text_
                [ x (String.fromFloat position.x), y (String.fromFloat position.y), textAnchor "middle", dy ".3em" ]
                [ Svg.text nodeId ]
            ]
