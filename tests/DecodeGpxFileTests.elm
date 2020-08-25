module DecodeGpxFileTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GpxFile
import Test exposing (..)
import Xml.Decode as XmlDecode


suite : Test
suite =
    describe "Decoding a gpx file's xml"
        [ testDecodingTrackPointLine
        , testDecodingFullGpx
        ]


testDecodingFullGpx : Test
testDecodingFullGpx =
    test "Decoding the full gpx document" <|
        \_ ->
            GpxFile.parseGpxData exampleGpx
                |> Expect.all
                    [ Maybe.map (.trackPoints >> List.length)
                        >> Maybe.withDefault -1
                        >> Expect.equal 5
                    , Maybe.map .name
                        >> Maybe.withDefault ""
                        >> Expect.equal "Cycling 8/21/20 2:53 pm"
                    , Maybe.map .time
                        >> Maybe.withDefault ""
                        >> Expect.equal "2020-08-21T19:53:12Z"
                    ]


testDecodingTrackPointLine : Test
testDecodingTrackPointLine =
    test "Decoding trkpt node" <|
        \_ ->
            XmlDecode.decodeString GpxFile.trkptLineDecoder trkptLine
                |> Expect.all
                    [ Result.map .time
                        >> Result.withDefault ""
                        >> Expect.equal "2020-08-21T19:53:12Z"
                    , Result.map .elevation
                        >> Result.withDefault ""
                        >> Expect.equal "185.0"
                    , Result.map .latitude
                        >> Result.withDefault ""
                        >> Expect.equal "42.007962000"
                    , Result.map .longitude
                        >> Result.withDefault ""
                        >> Expect.equal "-87.665366000"
                    ]


trkptLine : String
trkptLine =
    """
<trkpt lat="42.007962000" lon="-87.665366000"><ele>185.0</ele><time>2020-08-21T19:53:12Z</time></trkpt>
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
