module Commons.Constant exposing (..)

import Diagrams.Type exposing (NodeSize)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- reader monad for future possible inspiration https://gist.github.com/jliuhtonen/189facfd3841e0c1888b58caf4bfb9aa


const_START : String
const_START =
    "⒮"


const_END : String
const_END =
    "⒠"


const_VIEWBOX_PADDING : Float
const_VIEWBOX_PADDING =
    70


const_XY_OFFSET : Float
const_XY_OFFSET =
    150


const_START_END_NODE_RADIUS : Float
const_START_END_NODE_RADIUS =
    15


const_TEXT_LINE_HEIGHT : Float
const_TEXT_LINE_HEIGHT =
    15


const_NODE_BOX_WIDTH : Float
const_NODE_BOX_WIDTH =
    70


const_NODE_BOX_CORNER_RADIUS : Float
const_NODE_BOX_CORNER_RADIUS =
    3


const_SMALL_NODE_BOX_SIZE : NodeSize
const_SMALL_NODE_BOX_SIZE =
    { width = 30, height = 30 }


const_PERSON_NODE_SIZE : NodeSize
const_PERSON_NODE_SIZE =
    { width = 30, height = 70 }


const_SVG_ARROW : List (Svg msg)
const_SVG_ARROW =
    -- source: https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element/marker
    [ defs []
        [ marker
            [ id "arrow"
            , markerWidth "10"
            , markerHeight "7"
            , refX "10"
            , refY "3.5"
            , orient "auto"
            , markerUnits "strokeWidth"
            ]
            [ Svg.path
                [ d "M0,0 L0,7 L10,3.5 Z" -- arrow pat
                , fill "black"
                ]
                []
            ]
        ]
    ]


const_SVG_CIRCLE : List (Svg msg)
const_SVG_CIRCLE =
    [ defs []
        [ marker
            [ id "circle"
            , markerWidth "10"
            , markerHeight "10"
            , refX "5"
            , refY "5"
            , orient "auto"
            , markerUnits "strokeWidth"
            ]
            [ Svg.circle
                [ cx "5"
                , cy "5"
                , r "4"
                , fill "black"
                ]
                []
            ]
        ]
    ]
