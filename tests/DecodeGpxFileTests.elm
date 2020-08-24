module DecodeGpxFileTests exposing (suite)

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


type alias GpxFile =
    { trackPoints : List TrackPoint
    }


trkptLineDecoder : XmlDecode.Decoder TrackPoint
trkptLineDecoder =
    XmlDecode.map4 TrackPoint
        (XmlDecode.path [ "ele" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "time" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.stringAttr "lat")
        (XmlDecode.stringAttr "lon")


trksegDecoder : XmlDecode.Decoder (List TrackPoint)
trksegDecoder =
    XmlDecode.path [ "trkpt" ] <| XmlDecode.list trkptLineDecoder


gpxDecoder : XmlDecode.Decoder GpxFile
gpxDecoder =
    XmlDecode.map GpxFile
        (XmlDecode.path [ "trk", "trkseg" ] <| XmlDecode.single trksegDecoder)


suite : Test
suite =
    describe "Decoding a gpx file's xml"
        [ testDecodingTrackPointLine
        , testDecodingTrackSegmentLine
        , testDecodingFullGpx
        ]


testDecodingFullGpx : Test
testDecodingFullGpx =
    describe "Decoding the full gpx document"
        [ test "Decoding track segments" <|
            \_ ->
                XmlDecode.decodeString gpxDecoder exampleGpx
                    |> Result.map (.trackPoints >> List.length)
                    |> Result.withDefault -1
                    |> Expect.equal 5
        ]


testDecodingTrackSegmentLine : Test
testDecodingTrackSegmentLine =
    describe "Decoding trkseg node"
        [ test "Should get list of TrackPoint" <|
            \_ ->
                XmlDecode.decodeString trksegDecoder trksegLine
                    |> Result.map List.length
                    |> Result.withDefault -1
                    |> Expect.equal 2
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


trksegLine : String
trksegLine =
    """
<trkseg>
    <trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:12Z</time></trkpt>
    <trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:25Z</time></trkpt>
</trkseg>
"""


exampleGpx : String
exampleGpx =
    """
<gpx
  version="1.1"
  creator="Runkeeper - http://www.runkeeper.com"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.topografix.com/GPX/1/1"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"
  xmlns:gpxtpx="http://www.garmin.com/xmlschemas/TrackPointExtension/v1">
    <trk>
        <name><![CDATA[Cycling 8/21/20 2:53 pm]]></name>
        <time>2020-08-21T19:53:12Z</time>
        <trkseg>
            <trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:12Z</time></trkpt>
            <trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:25Z</time></trkpt>
            <trkpt lat="42.007944000" lon="-87.665227000"><ele>185.0</ele><time>2020-08-21T19:53:28Z</time></trkpt>
            <trkpt lat="42.007901000" lon="-87.665130000"><ele>185.0</ele><time>2020-08-21T19:53:30Z</time></trkpt>
            <trkpt lat="42.007837000" lon="-87.665056000"><ele>185.0</ele><time>2020-08-21T19:53:32Z</time></trkpt>
        </trkseg>
    </trk>
</gpx>
    """
