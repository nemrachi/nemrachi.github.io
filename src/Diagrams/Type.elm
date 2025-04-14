module Diagrams.Type exposing (Diagram, Edge, Node, NodeId, NodeSize)

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


type alias Diagram =
    Dict NodeId (List Node)
