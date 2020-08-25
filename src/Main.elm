module Main exposing (main)

{- Combine GPX Files From Local System
   Tasks Remaining
   [x] Set up Layout

   [x] Add Files
        [x] React to click
        [x] Bring up File.Select.files
        [x] Save the files that were selected
        [x] Show the files that were selected
   [ ] Remove Files
   [ ] Reorder Files

   [x] Parse Files (xml)
   [x] Show Parsed Information


   [ ] Export Combined File
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import GpxFile
import Html exposing (Html)
import Task



-- MODEL


type SelectedFile
    = Parsed File.File GpxFile.GpxFile
    | NotParsed File.File


type alias Model =
    { selectedFiles : List SelectedFile
    , combinedGpxFile : Maybe GpxFile.GpxFile
    }


type alias Flags =
    {}



-- SELECTEDFILE


isParsed : SelectedFile -> Bool
isParsed file =
    case file of
        NotParsed _ ->
            False

        Parsed _ _ ->
            True


rawFileName : SelectedFile -> String
rawFileName =
    rawFile >> File.name


rawFile : SelectedFile -> File.File
rawFile file =
    case file of
        NotParsed f ->
            f

        Parsed f _ ->
            f


gpxFile : SelectedFile -> Maybe GpxFile.GpxFile
gpxFile file =
    case file of
        NotParsed _ ->
            Nothing

        Parsed _ g ->
            Just g


gpxActivityName : SelectedFile -> String
gpxActivityName file =
    case file of
        NotParsed _ ->
            "Haven't parsed activity data"

        Parsed _ g ->
            g.name


gpxFileName : SelectedFile -> String
gpxFileName file =
    case file of
        NotParsed f ->
            File.name f

        Parsed f _ ->
            File.name f


gpxActivityStartTime : SelectedFile -> Maybe String
gpxActivityStartTime file =
    case file of
        NotParsed _ ->
            Nothing

        Parsed _ g ->
            Just g.time



-- INIT


init : Flags -> ( Model, Cmd Message )
init _ =
    ( { selectedFiles = []
      , combinedGpxFile = Nothing
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = "Combine GPX Files"
    , body =
        [ layout [] <|
            column [ height fill, width fill ] <|
                [ titleView
                , row [ height fill, width fill ] [ menuView, fileListView model.selectedFiles ]
                ]
        ]
    }


titleView : Element Message
titleView =
    row
        [ width fill
        , padding 20
        , Border.color (rgb 0 0 0)
        , Border.width 1
        ]
        [ el [ centerX ] <| text "Combine GPX Files" ]


menuView : Element Message
menuView =
    column
        [ height fill
        , Border.color (rgb 0 0 0)
        , Border.width 1
        , padding 20
        , spacing 10
        ]
        [ text "Things We Can Do"
        , buttonView AddFilesButtonPressed "Load Files"
        , buttonView ExportCombined "Download Combined"
        ]


buttonView : Message -> String -> Element Message
buttonView onPress label =
    Input.button
        [ padding 20
        , Border.width 2
        , Border.rounded 16
        , Border.color <| rgb255 0x50 0x50 0x50
        , Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 0xD0 0xD0 0xD0 }
        , Background.color <| rgb255 114 159 207
        , Font.color <| rgb255 0xFF 0xFF 0xFF
        , mouseOver
            [ Background.color <| rgb255 0xFF 0xFF 0xFF, Font.color <| rgb255 0 0 0 ]
        , focused
            [ Border.shadow { offset = ( 4, 4 ), size = 3, blur = 10, color = rgb255 114 159 207 } ]
        , centerX
        ]
        { onPress = Just onPress
        , label = text label
        }


fileListView : List SelectedFile -> Element Message
fileListView selectedFiles =
    column
        [ height fill
        , width fill
        , Border.color (rgb 0 0 0)
        , Border.width 1
        , padding 20
        , spacing 40
        ]
        [ el [ centerX ] <| text "Loaded Files"
        , filesView selectedFiles
        ]


filesView : List SelectedFile -> Element Message
filesView selectedFiles =
    column [ spacing 10 ] <|
        List.map fileView selectedFiles


fileView : SelectedFile -> Element Message
fileView selectedFile =
    text <| gpxFileName selectedFile ++ " - " ++ gpxActivityName selectedFile



-- MESSAGE


type Message
    = AddFilesButtonPressed
    | FilesSelected File.File (List File.File)
    | GpxFileParsed String String
    | ExportCombined



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        AddFilesButtonPressed ->
            ( model
            , File.Select.files [] FilesSelected
            )

        FilesSelected file files ->
            let
                modelWithNewFilesAdded =
                    addNewSelectedFiles (file :: files) model
            in
            ( modelWithNewFilesAdded
            , loadFileContents modelWithNewFilesAdded.selectedFiles
            )

        GpxFileParsed fileName contents ->
            let
                updatedFiles =
                    setFileParsed fileName contents model.selectedFiles
                        |> List.sortBy (gpxActivityStartTime >> Maybe.withDefault "")
            in
            ( { model
                | selectedFiles = updatedFiles
                , combinedGpxFile = combineGpxFiles updatedFiles
              }
            , Cmd.none
            )

        ExportCombined ->
            let
                downloadCmd =
                    model.combinedGpxFile
                        |> Maybe.map GpxFile.toString
                        |> Maybe.map (File.Download.string "combined.gpx" "text/gpx")
                        |> Maybe.withDefault Cmd.none
            in
            ( model, downloadCmd )


combineGpxFiles : List SelectedFile -> Maybe GpxFile.GpxFile
combineGpxFiles selectedFiles =
    let
        gpxFiles =
            List.filterMap gpxFile selectedFiles

        combinedTrackPoints =
            gpxFiles
                |> List.concatMap .trackPoints
    in
    List.head gpxFiles
        |> Maybe.map (\g -> { g | trackPoints = combinedTrackPoints })


loadFileContents : List SelectedFile -> Cmd Message
loadFileContents =
    List.filter (not << isParsed)
        >> List.map
            (\getContentsOfThisFile ->
                Task.perform (GpxFileParsed <| gpxFileName getContentsOfThisFile) (File.toString <| rawFile getContentsOfThisFile)
            )
        >> Cmd.batch


addNewSelectedFiles : List File.File -> Model -> Model
addNewSelectedFiles newFiles model =
    let
        existingFileNames =
            model.selectedFiles |> List.map rawFileName

        filesToAdd =
            newFiles
                |> List.filter (\file -> not <| List.member (File.name file) existingFileNames)
                |> List.map NotParsed
    in
    { model | selectedFiles = filesToAdd }


setFileParsed : String -> String -> List SelectedFile -> List SelectedFile
setFileParsed fileName contents selectedFiles =
    List.map
        (\file ->
            if gpxFileName file == fileName then
                case GpxFile.parseGpxData contents of
                    Just g ->
                        Parsed (rawFile file) g

                    Nothing ->
                        file

            else
                file
        )
        selectedFiles



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- MAIN


main : Program Flags Model Message
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
