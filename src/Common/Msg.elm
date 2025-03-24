module Common.Msg exposing (Msg(..))

import Common.Position exposing (Position)
import Diagram.Type exposing (NodeId)


type Msg
    = TextChange String
    | DragStart NodeId Position
    | DragAt Position
    | DragEnd
