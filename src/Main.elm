module Main exposing (Model, Msg(..), init, update, view)

import Html
import Html.Events
import Ini


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
            { model | input_text = s }


view model =
    Html.div []
        [ Html.button [ Html.Events.onClick Generate ] [ Html.text "Generate" ]
        , Html.textarea [] []
        , Html.span [ Html.Events.onInput InputUpdated ] [ Html.text model.output_text ]
        ]
