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
import Html exposing (Html)



-- MODEL


type alias Model =
    { selectedFiles : List File.File }


type alias Flags =
    {}



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


filesView : List File.File -> Element Message
filesView selectedFiles =
    column [ spacing 10 ] <| List.map (File.name >> text) selectedFiles


fileListView : List File.File -> Element Message
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



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        AddFilesButtonPressed ->
            ( model
            , File.Select.files [ "text/gpx" ] FilesSelected
            )

        FilesSelected file files ->
            ( { model | selectedFiles = file :: files ++ model.selectedFiles }
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
