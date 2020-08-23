module Main exposing (main)

{- Combine GPX Files From Local System
   Tasks Remaining
   [ ] Set up Layout

   [ ] Add Files
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
import Html exposing (Html)



-- MODEL


type alias Files =
    List String


loadedFiles : Files
loadedFiles =
    [ "Monday, August 24 12:30pm"
    , "Monday, August 24 1:15pm"
    ]


type alias Model =
    {}


type alias Flags =
    {}



-- INIT


init : Flags -> ( Model, Cmd Message )
init _ =
    ( Model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Message
view _ =
    { title = "Combine GPX Files"
    , body =
        [ bodyView
        ]
    }


buttonView : String -> Element Message
buttonView label =
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
        { onPress = Nothing
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
        , buttonView "Load Files"
        , buttonView "Export File"
        ]


filesView : Files -> Element Message
filesView files =
    column [ spacing 10 ] <| List.map text files


fileListView : Element Message
fileListView =
    column
        [ height fill
        , width fill
        , Border.color (rgb 0 0 0)
        , Border.width 1
        , padding 20
        , spacing 40
        ]
        [ el [ centerX ] <| text "Loaded Files"
        , filesView loadedFiles
        ]


menu : Element Message
menu =
    row
        [ width fill
        , padding 20
        , Border.color (rgb 0 0 0)
        , Border.width 1
        ]
        [ el [ centerX ] <| text "Combine GPX Files" ]


bodyView : Html Message
bodyView =
    layout [] <|
        column [ height fill, width fill ] <|
            [ menu
            , row [ height fill, width fill ] [ menuView, fileListView ]
            ]



-- MESSAGE


type Message
    = None



-- UPDATE


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    ( model, Cmd.none )



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
