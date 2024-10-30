module Diagram.Flowchart exposing (parseFlowchart, renderFlowchart)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Node =
    { id : String
    , label : String
    , x : Float
    , y : Float }

type alias Transition =
    { from : String
    , to : String }

type alias DiagramData =
    { nodes : List Node
    , transitions : List Transition }

parseFlowchart : String -> Maybe DiagramData
parseFlowchart input =
    if String.startsWith "flowchart" input then
        Just
            { nodes =
                [ { id = "A", label = "Christmas", x = 100, y = 100 }
                , { id = "B", label = "Go shopping", x = 250, y = 100 }
                , { id = "C", label = "Let me think", x = 400, y = 100 }
                ]
            , transitions =
                [ { from = "A", to = "B" }
                , { from = "B", to = "C" }
                ]
            }
    else
        Nothing


renderFlowchart : DiagramData -> Svg msg
renderFlowchart data =
    svg [ width "500", height "300" ]
        (List.concat
            [ List.map renderNode data.nodes
            , List.map (renderTransition data.nodes) data.transitions
            ]
        )


renderNode : Node -> Svg msg
renderNode node =
    g []
        [ rect [ x (String.fromFloat node.x), y (String.fromFloat node.y), width "100", height "50", fill "lightgreen" ] []
        , text_ [ x (String.fromFloat (node.x + 50)), y (String.fromFloat (node.y + 25)), textAnchor "middle", dy ".3em" ] [ text node.label ]
        ]


renderTransition : List Node -> Transition -> Svg msg
renderTransition nodes transition =
    let
        fromNode = List.head (List.filter (\n -> n.id == transition.from) nodes)
        toNode = List.head (List.filter (\n -> n.id == transition.to) nodes)
    in
    case (fromNode, toNode) of
        (Just from, Just to) ->
            line [ x1 (String.fromFloat (from.x + 100)), y1 (String.fromFloat (from.y + 25)), x2 (String.fromFloat to.x), y2 (String.fromFloat (to.y + 25)), stroke "black", strokeWidth "2" ] []
        _ ->
            text_ [] [ text "Error: Invalid transition" ]
