module Main exposing (main)

import Browser
import Html exposing (Html, div, textarea)
import Html.Attributes as Attr exposing (value, placeholder, style)
import Html.Events exposing (onInput)
import Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Diagram.FlowChart exposing (parseFlowchart, renderFlowchart)


-- MODEL

type alias Model =
    { userInput : String
    , diagramType : DiagramType
    }

type DiagramType
    = StateDiagram
    | Flowchart
    | Unknown

initialModel : Model
initialModel =
    { userInput = ""
    , diagramType = Unknown
    }


-- MESSAGES

type Msg
    = InputChange String


-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChange newText ->
            let
                diagramType =
                    detectDiagramType newText
            in
            { model | userInput = newText, diagramType = diagramType }


-- VIEW

view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ div [ style "width" "50%", style "padding" "10px" ]
            [ textarea
                [ placeholder "Enter diagram syntax..."
                , value model.userInput
                , onInput InputChange
                , style "width" "100%", style "height" "100vh"
                ]
                []
            ]
        , div [ style "width" "50%", style "padding" "10px", style "border-left" "1px solid #ccc" ]
            [ renderDiagram model ]
        ]


-- DETECT DIAGRAM TYPE

detectDiagramType : String -> DiagramType
detectDiagramType input =
    if String.startsWith "stateDiagram-v2" input then
        StateDiagram
    else if String.startsWith "flowchart TD" input then
        Flowchart
    else
        Unknown


-- RENDER DIAGRAM

renderDiagram : Model -> Html msg
renderDiagram model =
    case model.diagramType of
        StateDiagram ->
            case parseStateDiagram model.userInput of
                Just parsedData ->
                    renderStateDiagram parsedData
                Nothing ->
                    div [] [ Html.text "Invalid state diagram syntax" ]

        Flowchart ->
            case parseFlowchart model.userInput of
                Just parsedData ->
                    renderFlowchart parsedData
                Nothing ->
                    div [] [ Html.text "Invalid flowchart syntax" ]

        Unknown ->
            div [] [ Html.text "Please provide a valid diagram type" ]


-- MAIN

main =
    Browser.sandbox { init = initialModel, update = update, view = view }
