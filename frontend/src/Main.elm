module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Navigation
import Note exposing (Note)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    List Note


init : Navigation.Location -> ( Model, Cmd Msg )
init _ =
    ( [ Note 1 "title" "body" ], Cmd.none )


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (Result Http.Error (List Note))
    | NoteRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            model ! []

        NotesReceived result ->
            case result of
                Err e ->
                    let
                        _ =
                            Debug.log "Error getting note " e
                    in
                    model ! []

                Ok notes ->
                    notes ! []

        NoteRequested ->
            model ! [ Http.send NotesReceived (Http.get "/notes" Note.notesDecoder) ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick NoteRequested ] [ text "Get note" ]
        , div [] [ text <| toString model ]
        ]
