module DecodeGpxFileTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Xml.Decode as XmlDecode


type alias TrackPoint =
    { elevation : String
    , time : String
    , latitude : String
    , longitude : String
    }


trkptLineDecoder : XmlDecode.Decoder TrackPoint
trkptLineDecoder =
    XmlDecode.map4 TrackPoint
        (XmlDecode.path [ "ele" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "time" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.stringAttr "lat")
        (XmlDecode.stringAttr "lon")


suite : Test
suite =
    describe "Decoding a gpx file's xml"
        [ testDecodingTrackPointLine
        ]


testDecodingTrackPointLine : Test
testDecodingTrackPointLine =
    describe "Decoding trkpt node"
        [ test "Decoding time" <|
            \_ ->
                XmlDecode.decodeString trkptLineDecoder trkptLine
                    |> Result.map .time
                    |> Result.withDefault ""
                    |> Expect.equal "2020-08-21T19:53:12Z"
        , test "Decoding elevation" <|
            \_ ->
                XmlDecode.decodeString trkptLineDecoder trkptLine
                    |> Result.map .elevation
                    |> Result.withDefault ""
                    |> Expect.equal "185.0"
        , test "Decoding latitude" <|
            \_ ->
                XmlDecode.decodeString trkptLineDecoder trkptLine
                    |> Result.map .latitude
                    |> Result.withDefault ""
                    |> Expect.equal "42.007962000"
        , test "Decoding longitude" <|
            \_ ->
                XmlDecode.decodeString trkptLineDecoder trkptLine
                    |> Result.map .longitude
                    |> Result.withDefault ""
                    |> Expect.equal "-87.665366000"
        ]


trkptLine : String
trkptLine =
    """
<trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:12Z</time></trkpt>
"""
