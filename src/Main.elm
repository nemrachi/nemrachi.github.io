module Main exposing (main)

import Browser
import Css.Class
import Diagram.UseCaseDiagram exposing (..)
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

-- TODO https://sporto.github.io/elm-patterns/basic/impossible-states.html

type DiagramType
    = StateDiagram
    | UseCaseDiagram
    | Unknown

-- TODO stateless components https://dev.to/dwayne/stateless-and-stateful-components-no-reusable-views-in-elm-2kg0

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

-- TODO for the future https://sporto.github.io/elm-patterns/basic/builder-pattern.html

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

            UseCaseDiagram ->
                div [] [ Html.text "Use case diagram" ]

            Unknown ->
                div [] []

detectDiagramType : String -> DiagramType
detectDiagramType text =
    case getFirstLine text of
        "stateDiagram" ->
            StateDiagram

        "useCaseDiagram" ->
            UseCaseDiagram

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