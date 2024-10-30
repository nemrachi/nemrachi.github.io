module Main exposing (main)

import Browser
import Diagram.Flowchart exposing (parseFlowchart, renderFlowchart)
import Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (value, placeholder, class)
import Html.Events exposing (onInput)
import List exposing (..)


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
                diagramType = detectDiagramType newText
            in
                { model | diagramType = diagramType, userText = newText }


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "flex-container" ]
        [ div [ class "textarea-container" ]
            [ textarea
                [ placeholder "Enter code to generate diagram..."
                , value model.userText
                , onInput TextChange
                , class "textarea"
                ]
                []
            ]
        , div [ class "diagram-container" ]
            [ renderDiagram model ]
        ]


-- HELPER FUNCTIONS

detectDiagramType : String -> DiagramType
detectDiagramType text =
    case (getFirstLine text) of
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


renderDiagram : Model -> Html msg
renderDiagram model =
    case model.diagramType of
        StateDiagram ->
            case parseStateDiagram model.userText of
                Just parsedData ->
                    renderStateDiagram parsedData
                Nothing ->
                    div [] [ Html.text "Invalid state diagram syntax" ]

        Flowchart ->
            case parseFlowchart model.userText of
                Just parsedData ->
                    renderFlowchart parsedData
                Nothing ->
                    div [] [ Html.text "Invalid flowchart syntax" ]

        Unknown ->
            div [] []


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }