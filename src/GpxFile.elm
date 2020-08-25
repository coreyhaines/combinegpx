module GpxFile exposing (GpxFile, parseGpxData, toString, trkptLineDecoder)

import Xml.Decode as XmlDecode


type alias TrackPoint =
    { elevation : String
    , time : String
    , latitude : String
    , longitude : String
    }


type alias GpxFile =
    { name : String
    , time : String
    , trackPoints : List TrackPoint
    }


parseGpxData : String -> Maybe GpxFile
parseGpxData =
    XmlDecode.decodeString gpxDecoder
        >> Result.toMaybe


gpxDecoder : XmlDecode.Decoder GpxFile
gpxDecoder =
    XmlDecode.map3 GpxFile
        (XmlDecode.path [ "trk", "name" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "trk", "time" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "trk", "trkseg" ] <| XmlDecode.single trksegDecoder)


trksegDecoder : XmlDecode.Decoder (List TrackPoint)
trksegDecoder =
    XmlDecode.path [ "trkpt" ] <| XmlDecode.list trkptLineDecoder


trkptLineDecoder : XmlDecode.Decoder TrackPoint
trkptLineDecoder =
    XmlDecode.map4 TrackPoint
        (XmlDecode.path [ "ele" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "time" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.stringAttr "lat")
        (XmlDecode.stringAttr "lon")


headerToString : GpxFile -> String
headerToString file =
    "<name><![CDATA[{{name}}]]></name><time>{{time}}</time>"
        |> String.replace "{{name}}" file.name
        |> String.replace "{{time}}" file.time


trackPointToString : TrackPoint -> String
trackPointToString trackPoint =
    """
<trkpt lat="{{latitude}}" lon="{{longitude}}"><ele>{{elevation}}</ele><time>{{time}}</time></trkpt>
"""
        |> String.replace "{{latitude}}" trackPoint.latitude
        |> String.replace "{{longitude}}" trackPoint.longitude
        |> String.replace "{{elevation}}" trackPoint.elevation
        |> String.replace "{{time}}" trackPoint.time


trackSegmentsToString : GpxFile -> String
trackSegmentsToString file =
    "<trkseg>"
        ++ (file.trackPoints |> List.map trackPointToString |> List.foldr (++) "")
        ++ "</trkseg>"


toString : GpxFile -> String
toString file =
    """
<?xml version="1.0" encoding="UTF-8"?>
<gpx
  version="1.1"
  creator="Runkeeper - http://www.runkeeper.com"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns="http://www.topografix.com/GPX/1/1"
  xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"
  xmlns:gpxtpx="http://www.garmin.com/xmlschemas/TrackPointExtension/v1">
<trk>
"""
        ++ headerToString file
        ++ trackSegmentsToString file
        ++ """
</trk>
</gpx>
"""
