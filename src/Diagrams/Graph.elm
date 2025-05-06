module Diagrams.Graph exposing (Edge, Graph, LineParser, Node, NodeId, NodeSize, parseGraph)

import Dict exposing (Dict)



-- TYPES
-- TODO https://sporto.github.io/elm-patterns/basic/parse-dont-validate.html


type alias NodeId =
    String


type alias Edge =
    { from : NodeId
    , to : NodeId
    , label : Maybe String
    }


type alias Node =
    { name : NodeId
    , edgeLabel : Maybe String
    }


type alias NodeSize =
    { width : Float
    , height : Float
    }



-- https://sporto.github.io/elm-patterns/basic/wrap-early.html
-- TODO maybe phantom type (more of not)
-- https://sporto.github.io/elm-patterns/advanced/phantom-types.html


type alias Graph =
    Dict NodeId (List Node)



-- INTERFACES


type alias LineParser =
    String -> Maybe Edge



--- FUNCTIONS


parseGraph : LineParser -> List String -> Graph
parseGraph parser diagramLines =
    diagramLines
        |> List.filterMap parser
        |> buildGraph


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
