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

   [ ] Parse Files (xml)
   [ ] Show Parsed Information


   [ ] Export Combined File
-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Select
import GpxFile
import Html exposing (Html)
import Task



-- MODEL


type GpxFile
    = Parsed File.File GpxFile.GpxFile
    | NotParsed File.File


type alias Model =
    { selectedFiles : List GpxFile }


type alias Flags =
    {}


isParsed : GpxFile -> Bool
isParsed file =
    case file of
        NotParsed f ->
            False

        Parsed f _ ->
            True


rawFile : GpxFile -> File.File
rawFile file =
    case file of
        NotParsed f ->
            f

        Parsed f _ ->
            f


gpxFileName : GpxFile -> String
gpxFileName file =
    case file of
        NotParsed f ->
            File.name f

        Parsed f _ ->
            File.name f



-- INIT


init : Flags -> ( Model, Cmd Message )
init _ =
    ( { selectedFiles = [] }
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Message
view model =
    { title = "Combine GPX Files"
    , body =
        [ bodyView model
        ]
    }


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

        --, buttonView "Export File"
        ]


filesView : List GpxFile -> Element Message
filesView selectedFiles =
    column [ spacing 10 ] <| List.map (gpxFileName >> text) selectedFiles


fileListView : List GpxFile -> Element Message
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


titleView : Element Message
titleView =
    row
        [ width fill
        , padding 20
        , Border.color (rgb 0 0 0)
        , Border.width 1
        ]
        [ el [ centerX ] <| text "Combine GPX Files" ]


bodyView : Model -> Html Message
bodyView model =
    layout [] <|
        column [ height fill, width fill ] <|
            [ titleView
            , row [ height fill, width fill ] [ menuView, fileListView model.selectedFiles ]
            ]



-- MESSAGE


type Message
    = AddFilesButtonPressed
    | FilesSelected File.File (List File.File)
    | GpxFileParsed String String



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        AddFilesButtonPressed ->
            ( model
            , File.Select.files [ "text/gpx" ] FilesSelected
            )

        FilesSelected file files ->
            let
                modelWithNewFilesAdded =
                    { model | selectedFiles = List.map NotParsed (file :: files) ++ model.selectedFiles }
            in
            ( modelWithNewFilesAdded
            , Cmd.batch <|
                (modelWithNewFilesAdded
                    |> .selectedFiles
                    |> List.filter (not << isParsed)
                    |> List.map
                        (\getContentsOfThisFile ->
                            Task.perform (GpxFileParsed <| gpxFileName getContentsOfThisFile) (File.toString <| rawFile getContentsOfThisFile)
                        )
                )
            )

        GpxFileParsed fileName contents ->
            let
                parsedFiles =
                    List.map
                        (\file ->
                            if gpxFileName file == fileName then
                                case GpxFile.parseGpxData contents of
                                    Just gpxFile ->
                                        Parsed (rawFile file) gpxFile

                                    Nothing ->
                                        file

                            else
                                file
                        )
                        model.selectedFiles
            in
            ( { model | selectedFiles = parsedFiles }
            , Cmd.none
            )



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
