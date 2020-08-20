module Main exposing (main)

import Browser
import Html exposing (h1, text)
import Html.Attributes exposing (style)



-- MODEL


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
view model =
    { title = "Combine GPX Files"
    , body =
        [ h1 [ style "display" "flex", style "justify-content" "center" ]
            [ text "Combine GPX Files" ]
        ]
    }



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
