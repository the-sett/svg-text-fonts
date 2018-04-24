module TextDiagrams exposing (TextDiagram, Path, PathSpec, Options)

import EveryDict exposing (EveryDict)


{-| Denotes a diagram containing text with a function that can be used to correctly
calculate the width of all text in the diagram.
-}
type alias TextDiagram a =
    { a
        | labels : List PathSpec
        , pathsForLabels : EveryDict PathSpec Path
    }


type alias Path =
    { width : Float
    , path : String
    }


type alias PathSpec =
    { text : String
    , font : String
    , fontSize : Float
    }


type alias Options =
    { kerning : Maybe Bool
    , letterSpacing : Maybe Float
    }
