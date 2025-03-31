module Commons.Msg exposing (Msg(..))

import Commons.Position exposing (Position)
import Diagrams.Type exposing (NodeId)


type Msg
    = TextChange String
    | DragStart NodeId Position
    | DragAt Position
    | DragEnd
