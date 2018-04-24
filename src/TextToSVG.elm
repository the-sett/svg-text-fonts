module TextToSVG
    exposing
        ( Model
        , Msg
        , subscriptions
        , init
        , update
        , convertTextToSvg
        , TextRenderFunc
        , textAsPath
        , textAsText
        , TextAlignment(..)
        , TextToSVGPort
        , TextToSVGResponsePort
        , TextDiagram
        , Path
        , PathSpec
        , Options
        )

{-| Provides functionality to convert text into SVG paths.

This is implemented on top of the opentype.js library, and uses ports to handle
the native code interaction with this library, even though the text conversion
functions routines are pure functions and do not really need to work asynchronously.

The `update` cycle for this module must be linked into code that makes use of it,
including its subscriptions.


# Types describing SVG diagrams with text.

@docs TextDiagram, Path, PathSpec, Options


# Text to path conversion cycle.

@docs Model, Msg, subscriptions, init, update, convertTextToSvg


# SVG text rednering functions, with path or browser rendered implmenetations.

@docs TextRenderFunc, textAsPath, textAsText, TextAlignment


# Ports needed to request/subscribe to text to SVG conversion events.

@docs TextToSVGPort, TextToSVGResponsePort

-}

import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Json.Decode as Decode
import Json.Encode as Encode
import MultiDict exposing (MultiDict)
import Set exposing (Set)
import TypedSvg exposing (svg, g, circle, rect, text_, tspan, line, path)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x, y, x1, y1, x2, y2, rx, ry, width, height, fontSize)
import TypedSvg.Attributes exposing (viewBox, shapeRendering, fill, fillOpacity, stroke, strokeDasharray, strokeLinecap, strokeLinejoin, fontFamily, textAnchor, textRendering, color, d, transform)
import TypedSvg.Core exposing (svgNamespace, text, Svg, Attribute)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (px, Fill(..), ShapeRendering(..), Opacity(..), AnchorAlignment(..), StrokeLinecap(..), StrokeLinejoin(..), TextRendering(..), Transform(..))


{-| Denotes a diagram containing text with a function that can be used to correctly
calculate the width of all text in the diagram.
-}
type alias TextDiagram a =
    { a
        | labels : List PathSpec
        , pathsForLabels : EveryDict PathSpec Path
    }


{-| Encodes a string as an SVG path describing its outline, which can be used to
draw the text in SVG.
The width of the text is also given.
-}
type alias Path =
    { width : Float
    , path : String
    }


{-| Describes some text as a String along with a named font and size. This is sufficient
to describe the SVG path for the text uniquely.
-}
type alias PathSpec =
    { text : String
    , font : String
    , fontSize : Float
    }


{-| Options that can be set to control how text is rendered into SVG paths.
-}
type alias Options =
    { kerning : Maybe Bool
    , letterSpacing : Maybe Float
    }


{-| The internal state of the text conversion. This keeps track of which diagrams are
still to complete, and which `PathSpec`s.
-}
type alias Model a =
    { diagramsToSize : Dict Int (TextDiagram a)
    , textToSize : MultiDict Int PathSpec
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


addTextPathToDiagram : TextPath -> Model a -> ( MultiDict Int PathSpec, Dict Int (TextDiagram a) )
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
convertLabel : TextToSVGPort Msg -> PathSpec -> ( Model a, Cmd Msg ) -> ( Model a, Cmd Msg )
convertLabel textToSVGPort label ( model, cmds ) =
    ( { model | textToSize = MultiDict.insert model.id label model.textToSize }
    , Cmd.batch <| [ commandForLabel textToSVGPort label model.id, cmds ]
    )


commandForLabel : TextToSVGPort Msg -> PathSpec -> Int -> Cmd Msg
commandForLabel textToSVGPort label id =
    textToSVGPort
        { id = id
        , text = label.text
        , font = label.font
        , fontSize = label.fontSize
        , kerning = True
        , letterSpacing = 0.0
        }


{-| The possible horizontal alignments for text.
-}
type TextAlignment
    = LeftAlign
    | CenterAlign
    | RightAlign


{-| A type alias for the text rendering functions, as their types are quite longt.
-}
type alias TextRenderFunc msg =
    PathSpec
    -> EveryDict PathSpec Path
    -> TextAlignment
    -> Float
    -> Float
    -> List (Attribute msg)
    -> Svg msg


{-| Renders a PathSpec as an SVG path.
This will be geometrically accurate and stable under motion, but the text rendering will not be hinted
and look a bit rough. Use this when animating text.
-}
textAsPath : TextRenderFunc msg
textAsPath pathSpec pathLookup align xpos ypos attributes =
    let
        textPath =
            EveryDict.get pathSpec pathLookup
                |> Maybe.withDefault { width = 0, path = "" }

        xAlignmentAdjust =
            case align of
                LeftAlign ->
                    0.0

                CenterAlign ->
                    (textPath.width / 2)

                RightAlign ->
                    textPath.width
    in
        path
            ([ transform [ Translate (xpos - xAlignmentAdjust) ypos ]
             , d textPath.path
             ]
                ++ attributes
            )
            []


{-| Renders a PathSpec as SVG text rendered by the browser.
This will be hinted and rendered for maximum legibility. It will look crisp and clear
. It will have geometric aberations that show up under animation as a jittering about
of the text. Use this for static text.
-}
textAsText : TextRenderFunc msg
textAsText pathSpec pathLookup align xpos ypos attributes =
    let
        textPath =
            EveryDict.get pathSpec pathLookup
                |> Maybe.withDefault { width = 0, path = "" }
    in
        text_
            ([ fontFamily [ "Noto Sans" ]
             , fontSize pathSpec.fontSize
             , textAnchor <| textAlignToAnchorAlignment align
             , textRendering TextRenderingOptimizeLegibility
             ]
                ++ attributes
            )
            [ tspan [ x <| xpos, y <| ypos ]
                [ text pathSpec.text ]
            ]


textAlignToAnchorAlignment : TextAlignment -> AnchorAlignment
textAlignToAnchorAlignment align =
    case align of
        LeftAlign ->
            AnchorStart

        CenterAlign ->
            AnchorMiddle

        RightAlign ->
            AnchorEnd


{-| Defines the type of the text-to-svg port.
-}
type alias TextToSVGPort msg =
    TextToSVGRequest -> Cmd msg


{-| Defines the type of the text-to-svg response port.
-}
type alias TextToSVGResponsePort msg =
    (TextPath -> msg) -> Sub msg


{-| Defines the fields needed to make a request to convert text to SVG.
-}
type alias TextToSVGRequest =
    { id : Int
    , text : String
    , font : String
    , fontSize : Float
    , kerning : Bool
    , letterSpacing : Float
    }


{-| Defines the response to a request to convert text to SVG, including the
SVG path data and metrics for the text.
-}
type alias TextPath =
    { id : Int
    , baseline : Float
    , width : Float
    , height : Float
    , ascender : Float
    , descender : Float
    , pathData : String
    , request : TextToSVGRequest
    }


{-| Encode requests.
-}
encodeTextToSVGRequest : TextToSVGRequest -> Encode.Value
encodeTextToSVGRequest v =
    Encode.object
        [ ( "id", Encode.int v.id )
        , ( "text", Encode.string v.text )
        , ( "font", Encode.string v.font )
        , ( "fontSize", Encode.float v.fontSize )
        , ( "kerning", Encode.bool v.kerning )
        , ( "letterSpacing", Encode.float v.letterSpacing )
        ]


{-| Decode responses.
-}
decodeTextPath : Decode.Decoder TextPath
decodeTextPath =
    Decode.map8 TextPath
        (Decode.field "id" Decode.int)
        (Decode.field "baseline" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)
        (Decode.field "ascender" Decode.float)
        (Decode.field "descender" Decode.float)
        (Decode.field "pathData" Decode.string)
        (Decode.field "request"
            (Decode.map6 TextToSVGRequest
                (Decode.field "id" Decode.int)
                (Decode.field "text" Decode.string)
                (Decode.field "font" Decode.string)
                (Decode.field "fontSize" Decode.float)
                (Decode.field "kerning" Decode.bool)
                (Decode.field "letterSpacing" Decode.float)
            )
        )
