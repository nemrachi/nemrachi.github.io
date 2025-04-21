module Commons.Msg exposing (Msg(..))

import Commons.Position exposing (MousePosition, Position)
import Diagrams.Graph exposing (NodeId)


type Msg
    = TextChange String
    | DragStart NodeId Position MousePosition
    | DragAt MousePosition
    | DragEnd
