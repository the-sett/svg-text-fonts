module SVGTextRendering exposing (TextRenderFunc, textAsPath, textAsText, TextAlignment(..))

{-| Renders text as an SVG `text` element, or as a `path` element, with a common
type for these text rendering methods.

@docs TextRenderFunc, textAsPath, textAsText, TextAlignment

-}

import EveryDict exposing (EveryDict)
import TextDiagrams exposing (PathSpec, Path)
import TypedSvg exposing (svg, g, circle, rect, text_, tspan, line, path)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x, y, x1, y1, x2, y2, rx, ry, width, height, fontSize)
import TypedSvg.Attributes exposing (viewBox, shapeRendering, fill, fillOpacity, stroke, strokeDasharray, strokeLinecap, strokeLinejoin, fontFamily, textAnchor, textRendering, color, d, transform)
import TypedSvg.Core exposing (svgNamespace, text, Svg, Attribute)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (px, Fill(..), ShapeRendering(..), Opacity(..), AnchorAlignment(..), StrokeLinecap(..), StrokeLinejoin(..), TextRendering(..), Transform(..))


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
