module Main exposing (main)

import Browser
import Commons.Drag exposing (Drag, applyDragToPosition)
import Commons.Mouse exposing (onMouseMove, onMouseUp)
import Commons.Msg exposing (Msg(..))
import Commons.Position exposing (NodePositions)
import Css.Class
import Diagrams.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Diagrams.Type exposing (Diagram)
import Diagrams.UseCaseDiagram exposing (renderUseCaseDiagram)
import Dict
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import List



-- MODEL


type alias Model =
    { diagramType : DiagramType
    , userText : String
    , diagram : Diagram
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
      , diagram = Dict.empty
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
                diagramLines =
                    List.tail (String.lines text) |> Maybe.withDefault []

                ( newDiagram, newPositions ) =
                    case detectDiagramType text of
                        StateDiagram ->
                            parseStateDiagram diagramLines model.nodePositions

                        _ ->
                            ( Dict.empty, Dict.empty )
            in
            ( { model
                | diagramType = detectDiagramType text
                , userText = text
                , diagram = newDiagram
                , nodePositions = newPositions
              }
            , Cmd.none
            )

        DragStart nodeId pos ->
            ( { model | drag = Just (Drag pos pos nodeId) }
            , Cmd.none
            )

        DragAt pos ->
            ( { model | drag = Maybe.map (\d -> { d | start = pos }) model.drag }
            , Cmd.none
            )

        DragEnd ->
            case model.drag of
                Just _ ->
                    ( { model
                        | nodePositions = applyDragToPosition model.drag model.nodePositions
                        , drag = Nothing
                      }
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
    case model.diagramType of
        StateDiagram ->
            if Dict.isEmpty model.diagram then
                div [] [ Html.text "Invalid state diagram syntax" ]

            else
                renderStateDiagram model.diagram (applyDragToPosition model.drag model.nodePositions)

        UseCaseDiagram ->
            renderUseCaseDiagram

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
