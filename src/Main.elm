module Main exposing (Model, Msg(..), init, update, view)

import Browser
import Html
import Html.Attributes
import Html.Events
import Ini
import Platform


type Msg
    = Generate
    | InputUpdated String


type alias Model =
    { input_text : String
    , output_text : String
    }


init : Model
init =
    { input_text = ""
    , output_text = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Generate ->
            case Ini.parseIni model.input_text of
                Ok ini ->
                    { model | output_text = Debug.toString ini }

                Err e ->
                    { model | output_text = Debug.toString e }

        InputUpdated s ->
            Debug.log "model" { model | input_text = s }


view model =
    Html.div []
        [ Html.button [ Html.Events.onClick Generate ] [ Html.text "Generate" ]
        , Html.textarea [ Html.Attributes.cols 120, Html.Attributes.rows 20, Html.Attributes.id "input_text", Html.Events.onInput InputUpdated ] []
        , Html.span [] [ Html.text model.output_text ]
        ]


main : Platform.Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
