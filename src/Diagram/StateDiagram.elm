module Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)

import Common.Constant exposing (const_END, const_NODE_RADIUS_STR, const_START, const_SVG_ARROW)
import Common.Graphics exposing (calculateArrowPoints, calculateViewBoxSize)
import Common.Mouse exposing (onMouseDown)
import Common.Msg exposing (Msg(..))
import Common.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ONE, const_POSITION_ZERO, getNodePosition)
import Diagram.Type exposing (Diagram, Edge, Node, NodeId)
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


parseStateDiagram : List String -> ( Diagram, NodePositions )
parseStateDiagram diagramLines =
    diagramLines
        |> List.filterMap parseLine
        -- TODO https://sporto.github.io/elm-patterns/basic/unwrap-maybe-early.html
        |> buildDiagram
        |> (\diagram -> ( diagram, calculatePositions const_START const_POSITION_ZERO diagram Dict.empty ))



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



-- TODO show error (Result, Error implementation)


parsePoint : String -> String -> NodeId
parsePoint point const =
    case point of
        "[*]" ->
            const

        _ ->
            point


parseEdgeLabel : List String -> Maybe String
parseEdgeLabel words =
    case words of
        ":" :: rest ->
            Just (String.join " " rest)

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
renderTransition parent positions child =
    let
        parentPosition =
            getNodePosition parent positions

        childPosition =
            Dict.get child.name positions |> Maybe.withDefault const_POSITION_ONE

        ( start, end ) =
            calculateArrowPoints parentPosition childPosition
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



-- TODO https://sporto.github.io/elm-patterns/basic/arguments-list.html


renderNode : NodeId -> Position -> List (Svg Msg)
renderNode nodeId position =
    let
        posX =
            String.fromFloat position.x

        posY =
            String.fromFloat position.y
    in
    [ circle
        [ cx posX
        , cy posY
        , r const_NODE_RADIUS_STR
        , fill "lightgreen"
        , onMouseDown nodeId
        ]
        []
    , text_
        [ x posX, y posY, textAnchor "middle", dy ".3em" ]
        [ Svg.text nodeId ]
    ]
