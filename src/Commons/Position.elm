module Commons.Position exposing (NodePositions, Position, calculatePositions, const_POSITION_ONE, const_POSITION_ZERO, getNodePosition)

import Commons.Constant exposing (const_XY_OFFSET)
import Commons.Dict exposing (pop)
import Diagrams.Type exposing (Diagram, Node, NodeId)
import Dict exposing (Dict)


type alias Position =
    { x : Float
    , y : Float
    }


const_POSITION_ZERO : Position
const_POSITION_ZERO =
    { x = 0, y = 0 }


const_POSITION_ONE : Position
const_POSITION_ONE =
    { x = 0, y = const_XY_OFFSET }


type alias NodePositions =
    Dict NodeId Position


getNodePosition : NodeId -> NodePositions -> Position
getNodePosition nodeId nodePositions =
    Dict.get nodeId nodePositions |> Maybe.withDefault const_POSITION_ZERO


calculatePositions : NodeId -> Position -> Diagram -> NodePositions -> NodePositions
calculatePositions parentId position diagram visited =
    let
        ( visitedParent, parentPosition ) =
            case Dict.get parentId visited of
                Just pos ->
                    ( visited, pos )

                Nothing ->
                    let
                        uniquePosition =
                            ensureUniquePosition position visited

                        updatedVisited =
                            Dict.insert parentId uniquePosition visited
                    in
                    ( updatedVisited, uniquePosition )

        ( children, remainingDiagram ) =
            pop parentId diagram

        visitedChildren =
            case children of
                Just childNodes ->
                    assignChildPositions parentPosition childNodes visitedParent

                Nothing ->
                    visitedParent

        nextParent =
            List.head (Dict.keys remainingDiagram)
    in
    case nextParent of
        Just next ->
            calculatePositions next const_POSITION_ONE remainingDiagram visitedChildren

        Nothing ->
            visitedChildren



-- HELPERS


assignChildPositions : Position -> List Node -> NodePositions -> NodePositions
assignChildPositions parentPosition children visited =
    let
        halfCount =
            round (toFloat (List.length children) / 3)

        ( _, ( _, finalPositions ) ) =
            List.foldl
                (\node ( index, ( currentVisited, newPositions ) ) ->
                    let
                        ( updatedVisited, updatedPositions ) =
                            processChild ( index, ( currentVisited, newPositions ) ) halfCount parentPosition node
                    in
                    ( index + 1, ( updatedVisited, updatedPositions ) )
                )
                ( 0, ( visited, Dict.empty ) )
                children
    in
    Dict.union visited finalPositions


processChild : ( Int, ( NodePositions, NodePositions ) ) -> Int -> Position -> Node -> ( NodePositions, NodePositions )
processChild ( index, ( currentVisited, newPositions ) ) halfCount parentPos childNode =
    let
        offset =
            toFloat (index - halfCount)

        candidatePosition =
            calculateNodePosition childNode.name parentPos offset currentVisited

        uniquePosition =
            ensureUniquePosition candidatePosition currentVisited

        updatedVisited =
            Dict.insert childNode.name uniquePosition currentVisited

        updatedPositions =
            Dict.insert childNode.name uniquePosition newPositions
    in
    ( updatedVisited, updatedPositions )


calculateNodePosition : NodeId -> Position -> Float -> NodePositions -> Position
calculateNodePosition nodeId parentPos offset visited =
    case Dict.get nodeId visited of
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
