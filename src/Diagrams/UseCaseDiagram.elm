module Diagrams.UseCaseDiagram exposing (parseUseCaseDiagram, renderUseCaseDiagram)

import Commons.Constant exposing (const_NODE_BOX_CORNER_RADIUS, const_NODE_COLOR, const_PERSON_NODE_SIZE, const_SVG_ARROW)
import Commons.Drag exposing (preserveDraggedPositions)
import Commons.Graphics as Graphics
import Commons.Msg exposing (Msg)
import Commons.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ZERO)
import Commons.TextParser exposing (getFirstParentNode, parseEdgeLabel)
import Diagrams.Type exposing (Edge, Graph, Node, NodeId)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- TODO  sizable ovals for texts (or diff shape) an wrappable text


parseUseCaseDiagram : List String -> NodePositions -> ( Graph, NodePositions )
parseUseCaseDiagram diagramLines nodePositions =
    diagramLines
        |> List.filterMap parseLine
        |> buildGraph
        |> (\graph -> ( graph, calculatePositions (getFirstParentNode diagramLines) const_POSITION_ZERO graph Dict.empty ))
        |> (\( graph, generatedPositions ) -> ( graph, preserveDraggedPositions nodePositions generatedPositions ))



-- GET EDGES


parseLine : String -> Maybe Edge
parseLine line =
    case String.words line of
        from :: "-->" :: to :: label ->
            Just
                { from = from
                , to = to
                , label = parseEdgeLabel label
                }

        _ ->
            Nothing



-- GET DIAGRAM


buildGraph : List Edge -> Graph
buildGraph edges =
    List.foldl addEdgeToDiagram Dict.empty edges


addEdgeToDiagram : Edge -> Graph -> Graph
addEdgeToDiagram edge graph =
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
        graph



-- RENDERING


renderUseCaseDiagram : Graph -> NodePositions -> Svg Msg
renderUseCaseDiagram graph positions =
    let
        vb =
            Graphics.calculateViewBoxSize positions

        actorIds =
            Dict.keys graph
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
                (Dict.toList graph)
            ++ List.concatMap
                (\( node, position ) ->
                    renderNode actorIds node position
                )
                (Dict.toList positions)
        )


renderTransition : NodeId -> NodePositions -> Node -> List (Svg msg)
renderTransition parentId positions child =
    Graphics.arrowLine parentId const_PERSON_NODE_SIZE positions child (Graphics.calculateNodeSize child.name)


renderNode : List NodeId -> NodeId -> Position -> List (Svg Msg)
renderNode actorIds nodeId position =
    if List.member nodeId actorIds then
        [ Graphics.personIconWithText nodeId position ]

    else
        [ Graphics.draggableRoundedBoxWithText nodeId position const_NODE_BOX_CORNER_RADIUS const_NODE_COLOR ]
