module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Html
import Http exposing (Error(..))
import Notes exposing (Notes)
import RemoteData exposing (RemoteData(..), WebData)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


type alias Model =
    WebData Notes.Model


type Msg
    = NotesReceived (WebData Notes)
    | NotesMsg Notes.Msg
    | UrlChange Url
    | UrlRequest Browser.UrlRequest


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( RemoteData.Loading
    , Notes.getNotes NotesReceived
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NotesReceived notesWebData ->
            ( RemoteData.map Notes.init notesWebData
            , Cmd.none
            )

        NotesMsg notesMsg ->
            case RemoteData.map (Notes.update NotesReceived notesMsg) model of
                Success ( newNotes, cmd ) ->
                    ( Success newNotes, cmd )

                _ ->
                    ( model, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                RemoteData.Loading ->
                    Html.text "Loading notes ..."

                RemoteData.NotAsked ->
                    Html.text "We should never end up in 'NotAsked' state"

                RemoteData.Failure error ->
                    Html.text ("Error loading notes: " ++ httpErrorToString error)

                RemoteData.Success notesModel ->
                    Html.map NotesMsg <| Notes.view notesModel
    in
    { title = "Personal Knowledge Base"
    , body = [ body ]
    }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    "Http Error : "
        ++ (case error of
                BadUrl x ->
                    "Bad Url : " ++ x

                Timeout ->
                    "Time"

                NetworkError ->
                    "NetworkError"

                BadStatus statusCode ->
                    "BadStatus " ++ String.fromInt statusCode

                BadBody bodyStr ->
                    "BadBody " ++ bodyStr
           )
