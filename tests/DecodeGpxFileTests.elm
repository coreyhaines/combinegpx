module DecodeGpxFileTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Xml.Decode as XmlDecode


type alias TrackPoint =
    { time : String
    }


trkptLineDecoder : XmlDecode.Decoder TrackPoint
trkptLineDecoder =
    XmlDecode.map TrackPoint
        (XmlDecode.path [ "time" ] <| XmlDecode.single XmlDecode.string)


suite : Test
suite =
    describe "Decoding a gpx file's xml"
        [ testDecodingTrackPointLine
        ]


testDecodingTrackPointLine : Test
testDecodingTrackPointLine =
    test "Decoding trkpt node" <|
        \_ ->
            XmlDecode.decodeString trkptLineDecoder trkptLine
                |> Result.map .time
                |> Result.withDefault ""
                |> Expect.equal "2020-08-21T19:53:12Z"


trkptLine : String
trkptLine =
    """
<trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:12Z</time></trkpt>
"""
