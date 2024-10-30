module Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Node =
    { id : String
    , x : Float
    , y : Float
    }

type alias Transition =
    { from : String
    , to : String }

type alias DiagramData =
    { nodes : List Node
    , transitions : List Transition }

parseStateDiagram : String -> Maybe DiagramData
parseStateDiagram input =
    -- Simplified parser
    if String.startsWith "stateDiagram" input then
        Just
        { nodes =
            [ { id = "Still", x = 150, y = 100 }
            , { id = "Moving", x = 300, y = 100 }
            , { id = "Crash", x = 450, y = 100 }
            ]
        , transitions =
            [ { from = "Still", to = "Moving" }
            , { from = "Moving", to = "Crash" }
            ]
        }
    else
        Nothing


renderStateDiagram : DiagramData -> Svg msg
renderStateDiagram data =
    svg [ width "500", height "300" ]
        (List.concat
            [ List.map renderNode data.nodes
            , List.map (renderTransition data.nodes) data.transitions
            ]
        )


renderNode : Node -> Svg msg
renderNode node =
    g []
        [ circle [ cx (String.fromFloat node.x), cy (String.fromFloat node.y), r "30", fill "lightblue" ] []
        , text_ [ x (String.fromFloat node.x), y (String.fromFloat node.y), textAnchor "middle", dy ".3em" ] [ text node.id ]
        ]


renderTransition : List Node -> Transition -> Svg msg
renderTransition nodes transition =
    let
        fromNode = List.head (List.filter (\n -> n.id == transition.from) nodes)
        toNode = List.head (List.filter (\n -> n.id == transition.to) nodes)
    in
    case (fromNode, toNode) of
        (Just from, Just to) ->
            line [ x1 (String.fromFloat (from.x + 30)), y1 (String.fromFloat from.y), x2 (String.fromFloat (to.x - 30)), y2 (String.fromFloat to.y), stroke "black", strokeWidth "2" ] []
        _ ->
            text_ [] [ text "Error: Invalid transition" ]
