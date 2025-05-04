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
    { diagramStrategy : DiagramStrategy
    , userText : String
    , graph : Graph
    , nodePositions : NodePositions
    , drag : Maybe Drag
    }



-- STRATEGIES


type alias DiagramStrategy =
    { parse : List String -> ( Graph, NodePositions )
    , render : Graph -> NodePositions -> Html Msg
    }


stateDiagramStrategy : DiagramStrategy
stateDiagramStrategy =
    { parse = parseStateDiagram
    , render = renderStateDiagram
    }


useCaseDiagramStrategy : DiagramStrategy
useCaseDiagramStrategy =
    { parse = parseUseCaseDiagram
    , render = renderUseCaseDiagram
    }


noStrategy : DiagramStrategy
noStrategy =
    { parse = \_ -> ( Dict.empty, Dict.empty )
    , render = \_ _ -> renderExampleText
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { diagramStrategy = noStrategy
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
                diagramStrategy =
                    detectDiagramStrategy text

                diagramLines =
                    List.tail (String.lines text) |> Maybe.withDefault []

                ( newGraph, newPositions ) =
                    diagramStrategy.parse diagramLines
            in
            ( { model
                | diagramStrategy = diagramStrategy
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
    if Dict.isEmpty model.graph then
        renderExampleText

    else
        model.diagramStrategy.render model.graph model.nodePositions


renderExampleText : Html Msg
renderExampleText =
    div [] [ Html.pre [] [ Html.text const_EXAMPLE_TEXT ] ]


detectDiagramStrategy : String -> DiagramStrategy
detectDiagramStrategy text =
    case getFirstLine text of
        "stateDiagram" ->
            stateDiagramStrategy

        "useCaseDiagram" ->
            useCaseDiagramStrategy

        _ ->
            noStrategy


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
