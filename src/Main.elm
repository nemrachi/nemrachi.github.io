module Main exposing (main)

import Browser
import Commons.Constant exposing (const_EXAMPLE_TEXT)
import Commons.Drag exposing (Drag, applyDragToPosition, onMouseMove, onMouseUp)
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (NodePositions, preservePositionsUniquely)
import Css.Class
import Diagrams.Graph exposing (Graph)
import Diagrams.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Diagrams.UseCaseDiagram exposing (parseUseCaseDiagram, renderUseCaseDiagram)
import Dict
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import List



-- MODEL


type alias Model =
    { diagramType : DiagramType
    , userText : String
    , graph : Graph
    , nodePositions : NodePositions
    , drag : Maybe Drag
    }



-- TODO https://sporto.github.io/elm-patterns/basic/impossible-states.html


type DiagramType
    = StateDiagram
    | UseCaseDiagram
    | Unknown



-- TODO stateless components https://dev.to/dwayne/stateless-and-stateful-components-no-reusable-views-in-elm-2kg0


init : () -> ( Model, Cmd Msg )
init _ =
    ( { diagramType = Unknown
      , userText = ""
      , graph = Dict.empty
      , nodePositions = Dict.empty
      , drag = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChange text ->
            let
                diagramType =
                    detectDiagramType text

                diagramLines =
                    List.tail (String.lines text) |> Maybe.withDefault []

                ( newGraph, newPositions ) =
                    case diagramType of
                        StateDiagram ->
                            parseStateDiagram diagramLines

                        UseCaseDiagram ->
                            parseUseCaseDiagram diagramLines

                        _ ->
                            ( Dict.empty, Dict.empty )
            in
            ( { model
                | diagramType = diagramType
                , userText = text
                , graph = newGraph
                , nodePositions = preservePositionsUniquely model.nodePositions newPositions
              }
            , Cmd.none
            )

        DragStart nodeId nodePos mousePos ->
            ( { model | drag = Just (Drag mousePos mousePos nodeId nodePos) }
            , Cmd.none
            )

        DragAt pos ->
            let
                drag =
                    Maybe.map (\d -> { d | current = pos }) model.drag
            in
            ( { model
                | nodePositions = applyDragToPosition drag model.nodePositions
                , drag = drag
              }
            , Cmd.none
            )

        DragEnd ->
            case model.drag of
                Just _ ->
                    ( { model | drag = Nothing }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )



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
-- vo funkcionalnom pristupe by bolo komplikovane implementovat antivzor


renderDiagram : Model -> Html Msg
renderDiagram model =
    if Dict.isEmpty model.graph then
        renderExampleText

    else
        case model.diagramType of
            StateDiagram ->
                renderStateDiagram model.graph model.nodePositions

            UseCaseDiagram ->
                renderUseCaseDiagram model.graph model.nodePositions

            Unknown ->
                renderExampleText


renderExampleText : Html Msg
renderExampleText =
    div [] [ Html.pre [] [ Html.text const_EXAMPLE_TEXT ] ]


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ onMouseMove, onMouseUp ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
