module Diagrams.UseCaseDiagram exposing (parseUseCaseDiagram, renderUseCaseDiagram)

import Commons.Constant exposing (const_NODE_RADIUS, const_SVG_ARROW)
import Commons.Dict exposing (getFirstKey)
import Commons.Drag exposing (preserveDraggedPositions)
import Commons.Graphics as Graphics
import Commons.Msg exposing (Msg)
import Commons.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ZERO)
import Commons.TextParser exposing (parseEdgeLabel)
import Diagrams.Type exposing (Diagram, Edge, Node, NodeId)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- TODO  sizable ovals for texts (or diff shape) an wrappable text


parseUseCaseDiagram : List String -> NodePositions -> ( Diagram, NodePositions )
parseUseCaseDiagram diagramLines nodePositions =
    diagramLines
        |> List.filterMap parseLine
        |> buildDiagram
        |> (\diagram -> ( diagram, calculatePositions (getFirstKey diagram |> Maybe.withDefault "") const_POSITION_ZERO diagram Dict.empty ))
        |> (\( diagram, generatedPositions ) -> ( diagram, preserveDraggedPositions nodePositions generatedPositions ))



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


renderUseCaseDiagram : Diagram -> NodePositions -> Svg Msg
renderUseCaseDiagram diagram positions =
    let
        vb =
            Graphics.calculateViewBoxSize positions

        actorIds =
            Dict.keys diagram
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
                (Dict.toList diagram)
            ++ List.concatMap
                (\( node, position ) ->
                    renderNode actorIds node position
                )
                (Dict.toList positions)
        )


renderTransition : NodeId -> NodePositions -> Node -> List (Svg msg)
renderTransition parentId positions child =
    Graphics.arrowLine parentId const_NODE_RADIUS positions child const_NODE_RADIUS


renderNode : List NodeId -> NodeId -> Position -> List (Svg Msg)
renderNode actorIds nodeId position =
    if List.member nodeId actorIds then
        [ Graphics.personIcon nodeId position
        , Graphics.noSelectableText nodeId (Position position.x (position.y + 50))
        ]

    else
        [ Graphics.draggableRoundedBoxWithText nodeId position 70 3 "lightgreen" ]
