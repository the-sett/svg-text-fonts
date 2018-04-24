module SVGTextSizing
    exposing
        ( Model
        , Msg
        , subscriptions
        , init
        , update
        , convertTextToSvg
        )

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import MultiDict exposing (MultiDict)
import Porter
import Set exposing (Set)
import SVGTextPort exposing (TextToSVGRequest, TextPath, textToSVG, textToSVGResponse)
import TextDiagrams exposing (TextDiagram)


type alias Model a =
    { diagramsToSize : Dict Int (TextDiagram a)
    , textToSize : MultiDict Int TextDiagrams.PathSpec
    , id : Int
    }


init : Model a
init =
    { diagramsToSize = Dict.empty
    , textToSize = Dict.empty
    , id = 0
    }


type Msg
    = FontMetrics TextPath


subscriptions : Model a -> Sub Msg
subscriptions model =
    Sub.batch
        [ textToSVGResponse FontMetrics ]


{-| Handles updates from the text to SVG return port, that provide text converted to SVG
with sizing information.
-}
update : Msg -> Model a -> ( Model a, Cmd Msg, List (TextDiagram a) )
update action model =
    case action of
        FontMetrics textPath ->
            let
                ( remainingText, updatedDiagrams ) =
                    addTextPathToDiagram textPath model

                ( remainingDiagrams, completedDiagrams ) =
                    findCompletedDiagrams { model | diagramsToSize = updatedDiagrams, textToSize = remainingText }
            in
                ( { model | diagramsToSize = remainingDiagrams, textToSize = remainingText }
                , Cmd.none
                , completedDiagrams
                )


addTextPathToDiagram : TextPath -> Model a -> ( MultiDict Int TextDiagrams.PathSpec, Dict Int (TextDiagram a) )
addTextPathToDiagram textPath model =
    let
        maybeDiagram =
            Dict.get textPath.id model.diagramsToSize

        id =
            textPath.id

        text =
            textPath.request.text

        pathSpec =
            { text = text
            , font = textPath.request.font
            , fontSize = textPath.request.fontSize
            }
    in
        case maybeDiagram of
            Nothing ->
                ( model.textToSize, model.diagramsToSize )

            Just diagram ->
                let
                    sizedDiagram =
                        { diagram
                            | pathsForLabels =
                                EveryDict.insert
                                    pathSpec
                                    { width = textPath.width
                                    , path = textPath.pathData
                                    }
                                    diagram.pathsForLabels
                        }
                in
                    ( MultiDict.remove id pathSpec model.textToSize
                    , Dict.insert id sizedDiagram model.diagramsToSize
                    )


{-| For each diagram id that has no more labels to size, remove that diagram from the
set of diagrams needing to be sized, and report it in the results list.
-}
findCompletedDiagrams : Model a -> ( Dict Int (TextDiagram a), List (TextDiagram a) )
findCompletedDiagrams model =
    Dict.foldl
        (\id diagram ( dict, list ) ->
            if (MultiDict.get id model.textToSize) == Nothing then
                ( Dict.remove id dict, diagram :: list )
            else
                ( dict, list )
        )
        ( model.diagramsToSize, [] )
        model.diagramsToSize


{-| Given a list of diagrams that need their text converted to SVG, and the current
state of the converter Model, produces a new model and a set of commands for the
requests on the text to SVG port, to do the conversion work. The new model contains
a set of diagrams needing to be sized, updated with the list of diagram requests to
be processed.
-}
convertTextToSvg : List (TextDiagram a) -> Model a -> ( Model a, Cmd Msg )
convertTextToSvg diagrams model =
    List.foldl convertDiagram ( model, Cmd.none ) diagrams


{-| For a diagram, updates the model with the diagram to be sized, and provides
commands to do the sizing operations.
-}
convertDiagram : TextDiagram a -> ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
convertDiagram diagram ( model, cmds ) =
    let
        diagramId =
            model.id + 1
    in
        List.foldl
            convertLabel
            ( { model
                | id = diagramId
                , diagramsToSize = Dict.insert diagramId diagram model.diagramsToSize
              }
            , cmds
            )
            diagram.labels


{-| For a single label within a diagram, updates the model with the diagram to be
sized, and provides commands to do the sizing operations.
-}
convertLabel : TextDiagrams.PathSpec -> ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
convertLabel label ( model, cmds ) =
    ( { model | textToSize = MultiDict.insert model.id label model.textToSize }
    , Cmd.batch <| [ commandForLabel label model.id, cmds ]
    )


commandForLabel : TextDiagrams.PathSpec -> Int -> Cmd Msg
commandForLabel label id =
    textToSVG
        { id = id
        , text = label.text
        , font = label.font
        , fontSize = label.fontSize
        , kerning = True
        , letterSpacing = 0.0
        }
