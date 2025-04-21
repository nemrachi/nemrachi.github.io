module Commons.TextParser exposing (getFirstParentNode, parseEdgeLabel, parsePoint, sliceTextLines)

import Diagrams.Graph exposing (NodeId)



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


getFirstParentNode : List String -> NodeId
getFirstParentNode diagramLines =
    diagramLines
        |> List.filterMap extractFirstWordHelper
        |> List.head
        |> Maybe.withDefault ""


sliceTextLines : Int -> String -> List String
sliceTextLines chunkSize str =
    let
        total =
            String.length str
    in
    sliceHelper chunkSize 0 total str



-- HELPERS


extractFirstWordHelper : String -> Maybe NodeId
extractFirstWordHelper line =
    case String.words line of
        source :: "-->" :: _ ->
            Just source

        _ ->
            Nothing


sliceHelper : Int -> Int -> Int -> String -> List String
sliceHelper chunkSize start total str =
    if start >= total then
        []

    else
        let
            end =
                Basics.min (start + chunkSize) total
        in
        String.slice start end str :: sliceHelper chunkSize end total str
