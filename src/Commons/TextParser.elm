module Commons.TextParser exposing (parseEdgeLabel, parsePoint, sliceTextLines)

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


sliceTextLines : Int -> String -> List String
sliceTextLines chunkSize str =
    let
        total =
            String.length str
    in
    sliceHelper chunkSize 0 total str


sliceHelper : Int -> Int -> Int -> String -> List String
sliceHelper chunkSize start total str =
    if start >= total then
        []

    else
        let
            end_ =
                Basics.min (start + chunkSize) total
        in
        String.slice start end_ str :: sliceHelper chunkSize end_ total str
