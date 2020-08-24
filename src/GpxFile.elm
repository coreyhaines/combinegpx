module GpxFile exposing (GpxFile, TrackPoint, gpxDecoder, parseGpxData, trkptLineDecoder, trksegDecoder)

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
parseGpxData gpxData =
    let
        decoded =
            XmlDecode.decodeString gpxDecoder gpxData
    in
    case decoded of
        Ok gpxFile ->
            Just gpxFile

        Err _ ->
            Nothing


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
    XmlDecode.map3 GpxFile
        (XmlDecode.path [ "trk", "name" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "trk", "time" ] <| XmlDecode.single XmlDecode.string)
        (XmlDecode.path [ "trk", "trkseg" ] <| XmlDecode.single trksegDecoder)
