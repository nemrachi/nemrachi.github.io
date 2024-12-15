module Main exposing (main)

import Browser
import Css.Class
import Diagram.Flowchart exposing (parseFlowchart, renderFlowchart)
import Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import List exposing (..)
import Dict exposing (..)


-- MODEL

type alias Model =
    { diagramType : DiagramType
    , userText : String
    }

type DiagramType
    = StateDiagram
    | Flowchart
    | Unknown


initialModel : Model
initialModel =
    { diagramType = Unknown
    , userText = ""
    }


-- MESSAGES

type Msg
    = TextChange String


-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChange newText ->
            let
                diagramType =
                    detectDiagramType newText
            in
            { model | diagramType = diagramType, userText = newText }


-- VIEW

view : Model -> Html Msg
view model =
    div Css.Class.flexContainer
        [ div Css.Class.texrAreaContainer
            [ textarea
                ([ placeholder "Enter code to generate diagram..."
                 , value model.userText
                 , onInput TextChange
                 ]
                 ++ Css.Class.textArea
                )
                []
            ]
        , div Css.Class.diagramContainer
            [ renderDiagram model ]
        ]


-- HELPER FUNCTIONS

renderDiagram : Model -> Html msg
renderDiagram model =
    let
        diagramLines = List.tail (String.lines model.userText) |> Maybe.withDefault []
    in
        case model.diagramType of
            StateDiagram ->
                let
                    (graph, positions) = parseStateDiagram diagramLines
                in
                    if Dict.isEmpty graph then
                        div [] [ Html.text "Invalid state diagram syntax" ]
                    else
                        renderStateDiagram graph positions

            Flowchart ->
                case parseFlowchart model.userText of
                    Just parsedData ->
                        renderFlowchart parsedData

                    Nothing ->
                        div [] [ Html.text "Invalid flowchart syntax" ]

            Unknown ->
                div [] []

detectDiagramType : String -> DiagramType
detectDiagramType text =
    case getFirstLine text of
        "stateDiagram" ->
            StateDiagram

        "flowchart" ->
            Flowchart

        _ ->
            Unknown

getFirstLine : String -> String
getFirstLine text =
    case String.lines text of
        first :: _ ->
            first

        _ ->
            ""


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }