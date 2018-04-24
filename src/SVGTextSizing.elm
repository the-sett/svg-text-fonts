module SVGTextSizing
    exposing
        ( Model
        , Msg
        , subscriptions
        , init
        , update
        , convertTextToSvg
        )

{-| Provides functionality to convert text into SVG paths.

This is implemented on top of the opentype.js library, and uses ports to handle
the native code interaction with this library, even though the text conversion
functions routines are pure functions and do not really need to work asynchronously.

The `update` cycle for this module must be linked into code that makes use of it,
including its subscriptions.


# Text to path conversion cycle

@docs Model, Msg, subscriptions, init, update, convertTextToSvg

-}

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import MultiDict exposing (MultiDict)
import Set exposing (Set)
import SVGTextSPI exposing (TextToSVGPort, TextToSVGResponsePort, TextToSVGRequest, TextPath)
import TextDiagrams exposing (TextDiagram)


{-| The internal state of the text conversion. This keeps track of which diagrams are
still to complete, and which `PathSpec`s.
-}
type alias Model a =
    { diagramsToSize : Dict Int (TextDiagram a)
    , textToSize : MultiDict Int TextDiagrams.PathSpec
    , id : Int
    }


{-| Createa a new empty initial state for the text conversion.
-}
init : Model a
init =
    { diagramsToSize = Dict.empty
    , textToSize = Dict.empty
    , id = 0
    }


{-| Describes the text rendering outcome events from the conversion ports.
-}
type Msg
    = FontMetrics TextPath


{-| Defines the subscription needed to listen for responses on the text conversion response port.
-}
subscriptions : TextToSVGResponsePort Msg -> Model a -> Sub Msg
subscriptions responsePort model =
    Sub.batch
        [ responsePort FontMetrics ]


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
convertTextToSvg : TextToSVGPort Msg -> List (TextDiagram a) -> Model a -> ( Model a, Cmd Msg )
convertTextToSvg textToSVGPort diagrams model =
    List.foldl (convertDiagram textToSVGPort) ( model, Cmd.none ) diagrams


{-| For a diagram, updates the model with the diagram to be sized, and provides
commands to do the sizing operations.
-}
convertDiagram : TextToSVGPort Msg -> TextDiagram a -> ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
convertDiagram textToSVGPort diagram ( model, cmds ) =
    let
        diagramId =
            model.id + 1
    in
        List.foldl
            (convertLabel textToSVGPort)
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
convertLabel : TextToSVGPort Msg -> TextDiagrams.PathSpec -> ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
convertLabel textToSVGPort label ( model, cmds ) =
    ( { model | textToSize = MultiDict.insert model.id label model.textToSize }
    , Cmd.batch <| [ commandForLabel textToSVGPort label model.id, cmds ]
    )


commandForLabel : TextToSVGPort Msg -> TextDiagrams.PathSpec -> Int -> Cmd Msg
commandForLabel textToSVGPort label id =
    textToSVGPort
        { id = id
        , text = label.text
        , font = label.font
        , fontSize = label.fontSize
        , kerning = True
        , letterSpacing = 0.0
        }
