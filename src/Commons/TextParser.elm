module Commons.TextParser exposing (..)

import Diagrams.Type exposing (NodeId)



-- TODO show error (Result, Error implementation)


parsePoint : String -> String -> NodeId
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
