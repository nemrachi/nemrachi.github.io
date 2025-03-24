module Main exposing (main)

import Browser
import Browser.Events
import Common.Drag exposing (Drag, applyDragToPosition)
import Common.Mouse exposing (positionDecoder)
import Common.Msg exposing (Msg(..))
import Common.Position exposing (NodePositions)
import Css.Class
import Diagram.StateDiagram exposing (parseStateDiagram, renderStateDiagram)
import Diagram.Type exposing (Diagram)
import Diagram.UseCaseDiagram exposing (renderUseCaseDiagram)
import Dict
import Html exposing (Html, div, textarea)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Json.Decode as D
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
                            parseStateDiagram diagramLines

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
            Debug.log "DragStart" model.nodePositions
                |> (\_ ->
                        ( { model | drag = Just (Drag pos pos nodeId) }
                        , Cmd.none
                        )
                   )

        DragAt pos ->
            Debug.log "DragAt" pos
                |> (\_ ->
                        ( { model | drag = Maybe.map (\d -> { d | start = pos }) model.drag }
                        , Cmd.none
                        )
                   )

        DragEnd ->
            Debug.log "DragEnd" model.nodePositions
                |> (\_ ->
                        case model.drag of
                            Just d ->
                                ( { model
                                    | nodePositions = applyDragToPosition model.drag model.nodePositions
                                    , drag = Nothing
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                -- fallback in case DragEnd arrives unexpectedly
                                ( model, Cmd.none )
                   )



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
            Sub.batch
                [ Browser.Events.onMouseMove (D.map DragAt positionDecoder)
                , Browser.Events.onMouseUp (D.succeed DragEnd)
                ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
