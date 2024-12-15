module Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)

import Helper.Constant exposing (const_START, const_END, const_XY_OFFSET, const_NODE_RADIUS, const_SVG_ARROW, const_SVG_CIRCLE)
import Helper.Dict as HelperDict
import Dict exposing (Dict)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- TYPES

type alias Edge =
    { from : String
    , to : String
    , label : Maybe String
    }

type alias Node =
    { name : String
    , edgeLabel : Maybe String
    }

type alias Graph =
    Dict String (List Node)

type alias Position =
    { x : Float
    , y : Float
    }

type alias NodePositions =
    Dict String Position


-- CONSTANTS

positionZero : Position
positionZero = { x = 0, y = 0 }

positionOne : Position
positionOne = { x = 0, y = const_XY_OFFSET }


-- PARSING

parseStateDiagram : List String -> (Graph, NodePositions)
parseStateDiagram diagramLines =
    let
        edges = List.filterMap parseLine diagramLines
        graph = buildGraph edges
        positions = calculatePositions graph const_START positionZero Dict.empty
    in
        (graph, positions)


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

parsePoint : String -> String -> String
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


-- GET GRAPH

buildGraph : List Edge -> Graph
buildGraph edges =
    List.foldl addEdgeToGraph Dict.empty edges

addEdgeToGraph : Edge -> Graph -> Graph
addEdgeToGraph edge graph =
    let
        parent = edge.from
        childNode = { name = edge.to, edgeLabel = edge.label }
    in
        Dict.update parent (\maybeNodes ->
            Just (childNode :: Maybe.withDefault [] maybeNodes)
        ) graph


-- GET POSITIONS

calculatePositions : Graph -> String -> Position -> NodePositions -> NodePositions
calculatePositions graph parent position visited =                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    let
        (visitedParent, parentPosition) = 
            case Dict.get parent visited of
                Just pos ->
                    (visited, pos)
                Nothing ->
                    let
                        uniquePosition = ensureUniquePosition position visited
                        updatedVisited = Dict.insert parent uniquePosition visited
                    in
                        (updatedVisited, uniquePosition)

        (children, remainingGraph) = HelperDict.pop parent graph

        visitedChildren =
            case children of
                Just childNodes ->
                    assignChildPositions childNodes parentPosition visitedParent
                Nothing ->
                    visitedParent

        nextParent = List.head (Dict.keys remainingGraph)
    in
        case nextParent of
            Just next ->
                calculatePositions remainingGraph next positionOne visitedChildren
            Nothing ->
                visitedChildren

assignChildPositions : List Node -> Position -> NodePositions -> NodePositions
assignChildPositions children parentPosition visited =
    let
        halfCount = round (toFloat (List.length children) / 3)

        processChild (index, (currentVisited, accPositions)) node =
            let
                candidatePosition = calculateNodePosition node.name parentPosition (toFloat (index - halfCount)) currentVisited
                _ = Debug.log ("__candidatePosition " ++ node.name) (candidatePosition)
                uniquePosition = ensureUniquePosition candidatePosition currentVisited
                _ = Debug.log ("__uniquePosition " ++ node.name) (uniquePosition)
                updatedVisited = Dict.insert node.name uniquePosition currentVisited
                updatedPositions = Dict.insert node.name uniquePosition accPositions
            in
                (updatedVisited, updatedPositions)

        (_, (_, finalPositions)) =
            List.foldl
                (\node (index, (currentVisited, accPositions)) ->
                    let
                        (updatedVisited, updatedPositions) = processChild (index, (currentVisited, accPositions)) node
                    in
                        (index + 1, (updatedVisited, updatedPositions))
                )
                (0, (visited, Dict.empty))
                children
    in
        Dict.union visited finalPositions

calculateNodePosition : String -> Position -> Float -> NodePositions -> Position
calculateNodePosition nodeName parentPos offset visited =
    case Dict.get nodeName visited of
        Just pos ->
            if parentPos.y >= pos.y then
                { x = offset * const_XY_OFFSET, y = parentPos.y + const_XY_OFFSET }
            else
                pos
        Nothing ->
            { x = offset * const_XY_OFFSET, y = parentPos.y + const_XY_OFFSET }

ensureUniquePosition : Position -> NodePositions -> Position
ensureUniquePosition position visited =
    if Dict.values visited |> List.any (\pos -> pos.x == position.x && pos.y == position.y) then
        ensureUniquePosition { position | x = position.x + const_XY_OFFSET } visited
    else
        position


-- RENDERING

renderStateDiagram : Graph -> NodePositions -> Svg msg
renderStateDiagram graph positions =
    let
        (minPos, maxPos) = calculateMinMax positions
        padding = 50
        minX = minPos.x - padding
        minY = minPos.y - padding
        maxX = maxPos.x + padding
        maxY = maxPos.y + padding

        vb = String.join " "
            [ String.fromFloat minX
            , String.fromFloat minY
            , String.fromFloat (maxX - minX)
            , String.fromFloat (maxY - minY)
            ]
    in
    svg [ viewBox vb
        , preserveAspectRatio "xMidYMid meet"
        , width "100%"
        , height "100%"
        ]
        ( const_SVG_ARROW
            ++ List.concatMap (\(parent, children) ->
                    List.concatMap (renderTransition parent positions) children
                ) (Dict.toList graph)
            ++ List.concatMap (\(node, position) -> renderNode node position) (Dict.toList positions)
        )

calculateMinMax : NodePositions -> (Position, Position)
calculateMinMax positions =
    Dict.values positions
        |> List.foldl
            (\pos (minPos, maxPos) ->
                ( { x = Basics.min minPos.x pos.x, y = Basics.min minPos.y pos.y }
                , { x = Basics.max maxPos.x pos.x, y = Basics.max maxPos.y pos.y }
                )
            )
            (positionZero, positionZero)


renderTransition : String -> NodePositions -> Node -> List (Svg msg)
renderTransition parent positions child =
    let
        parentPosition = Dict.get parent positions |> Maybe.withDefault positionZero
        childPosition = Dict.get child.name positions |> Maybe.withDefault positionOne
        (start, end) = calculateArrowPoints parentPosition childPosition
    in
        [ line [ x1 (String.fromFloat start.x), y1 (String.fromFloat start.y)
               , x2 (String.fromFloat end.x), y2 (String.fromFloat end.y)
               , stroke "black", strokeWidth "2", markerEnd "url(#arrow)"
               ] []
        ]

calculateArrowPoints : Position -> Position -> (Position, Position)
calculateArrowPoints parent child =
    let
        dx = child.x - parent.x
        dy = child.y - parent.y
        distance = sqrt (dx * dx + dy * dy)
        offsetX = (dx / distance) * const_NODE_RADIUS
        offsetY = (dy / distance) * const_NODE_RADIUS
    in
        ( { x = parent.x + offsetX, y = parent.y + offsetY }
        , { x = child.x - offsetX, y = child.y - offsetY }
        )

renderNode : String -> Position -> List (Svg msg)
renderNode node position =
    let
        posX = String.fromFloat position.x
        posY = String.fromFloat position.y
    in
        [ circle [ cx posX, cy posY, r (String.fromFloat const_NODE_RADIUS), fill "lightgreen" ] []
        , text_ [ x posX, y posY, textAnchor "middle", dy ".3em" ] [ Svg.text node ]
        ]