module TextDiagrams exposing (TextDiagram, Path, PathSpec, Options)

{-| The types for SVG text diagrams.

@docs TextDiagram, Path, PathSpec, Options

-}

import EveryDict exposing (EveryDict)


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
