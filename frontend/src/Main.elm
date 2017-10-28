module Main exposing (main)

import Html exposing (Html, div, h3, text)
import Http
import Markdown
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
    ( []
    , Http.send NotesReceived (Http.get "/notes" Note.notesDecoder)
    )


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (Result Http.Error (List Note))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
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


view : Model -> Html Msg
view model =
    div [] <| List.map viewNote model


viewNote : Note -> Html a
viewNote note =
    div []
        [ h3 [] [ text note.nTitle ]
        , Markdown.toHtml [] note.nBody
        ]
