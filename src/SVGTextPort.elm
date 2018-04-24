port module SVGTextPort
    exposing
        ( TextToSVGRequest
        , TextPath
        , textToSVG
        , textToSVGResponse
        )

{-| Ports needed to request/subscribe to text to SVG conversion events.
-}

import Json.Decode as Decode
import Json.Encode as Encode


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


{-| Requests that text is converted to SVG.
-}
port textToSVG : TextToSVGRequest -> Cmd msg


{-| Creates a subscription to listen for responses to requests to convert text
to SVG
-}
port textToSVGResponse : (TextPath -> msg) -> Sub msg
