module Main exposing (main)

import Browser
import Hash.Dict as Dict exposing (Dict)
import Highlight
import Html exposing (Html, button, details, div, input, summary, text, textarea)
import Html.Attributes exposing (class, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Markdown
import Note exposing (Note)
import RemoteData exposing (WebData)
import Requests
import Types exposing (Msg(..), Notes)
import Url.Parser exposing (Url)


main : Program () Model Msg
main =
    Browser.fullscreen
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , onNavigation = Just UrlChange
        , view = view
        }


type alias Model =
    AbstractModel (WebData Notes)


type alias ModelWithNotes =
    AbstractModel Notes


type alias AbstractModel a =
    { notes : a
    , searchQuery : SearchQuery
    , noteEditState : Maybe NoteEditState
    }


type
    NoteEditState
    -- Keep original note (to check if changes made) + String representing edit changes
    = NoteEditState Note String


type SearchQuery
    = SearchQuery String


init : Browser.Env () -> ( Model, Cmd Msg )
init _ =
    ( { notes = RemoteData.Loading
      , searchQuery = SearchQuery ""
      , noteEditState = Nothing
      }
    , Requests.getNotes
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange _ ->
            ( model, Cmd.none )

        NotesReceived notesWebData ->
            ( { model | notes = notesWebData }, Cmd.none )

        SetSearchQuery queryString ->
            ( setQueryString queryString model, Cmd.none )

        ClearSearchQuery ->
            ( setQueryString "" model, Cmd.none )

        EditNote nId ->
            ( { model
                | noteEditState =
                    model.notes
                        |> RemoteData.map
                            (\notes ->
                                Dict.get nId notes
                                    |> Maybe.map (\note -> NoteEditState note note.nBody)
                            )
                        |> RemoteData.withDefault Nothing
              }
            , Cmd.none
            )

        NoteBodyChange newText ->
            ( { model
                | noteEditState =
                    Maybe.map
                        (\(NoteEditState note _) -> NoteEditState note newText)
                        model.noteEditState
              }
            , Cmd.none
            )

        CancelNoteEdit ->
            ( { model | noteEditState = Nothing }, Cmd.none )

        SaveNoteEdit ->
            case model.noteEditState of
                Nothing ->
                    ( model, Cmd.none )

                Just editState ->
                    saveNote editState model


saveNote : NoteEditState -> Model -> ( Model, Cmd Msg )
saveNote (NoteEditState origNote newBody) model =
    let
        newNote =
            { origNote | nBody = newBody }

        command =
            if newBody == origNote.nBody then
                Cmd.none
            else
                Requests.postNote newNote
    in
    ( { model
        | noteEditState = Nothing
        , notes = RemoteData.map (Dict.insert newNote.nId newNote) model.notes
      }
    , command
    )


setQueryString :
    String
    -> { a | searchQuery : SearchQuery }
    -> { a | searchQuery : SearchQuery }
setQueryString queryString model =
    { model | searchQuery = SearchQuery queryString }


view : Model -> Browser.Page Msg
view model =
    let
        body =
            case model.notes of
                RemoteData.Loading ->
                    [ text "Loading notes ..." ]

                RemoteData.NotAsked ->
                    [ text "We should never end up in 'NotAsked' state" ]

                RemoteData.Failure httpError ->
                    let
                        _ =
                            Debug.log "Http error occurred" httpError
                    in
                    [ text "Http Error occurred. See dev console for details" ]

                RemoteData.Success loadedNotes ->
                    viewPage
                        { notes = loadedNotes
                        , searchQuery = model.searchQuery
                        , noteEditState = model.noteEditState
                        }
    in
    { title = "Personal Knowledge Base"
    , body = body
    }


viewPage : ModelWithNotes -> List (Html Msg)
viewPage model =
    [ searchBar model.searchQuery
    , viewNotes model
    ]


viewNotes : ModelWithNotes -> Html Msg
viewNotes model =
    model.notes
        |> Dict.values
        |> List.filter (noteMatchesQuery model.searchQuery)
        |> List.sortBy .nTitle
        |> List.map (viewNote model.noteEditState model.searchQuery)
        |> div []


searchBar : SearchQuery -> Html Msg
searchBar (SearchQuery queryString) =
    div []
        [ input [ type_ "text", onInput SetSearchQuery, value queryString ] []
        , button [ onClick ClearSearchQuery ] [ text "Clear" ]
        ]


viewNote : Maybe NoteEditState -> SearchQuery -> Note -> Html Msg
viewNote maybeEditState (SearchQuery searchQuery) note =
    let
        noteBodyView =
            Maybe.map
                (\(NoteEditState { nId } editedBodyText) ->
                    if note.nId == nId then
                        noteBodyEditor editedBodyText
                    else
                        markdownBody note
                )
                maybeEditState
                |> Maybe.withDefault (markdownBody note)

        markdownBody n =
            div [ class "note-body" ]
                [ Markdown.toHtml [] n.nBody
                , button [ class "note-button", onClick (EditNote n.nId) ] [ text "Edit" ]
                ]
    in
    details []
        [ summary [ class "note-title" ] [ Highlight.highlight searchQuery note.nTitle ]
        , noteBodyView
        ]


noteBodyEditor : String -> Html Msg
noteBodyEditor editedBodyText =
    div []
        [ textarea
            [ class "note-edit-area"
            , rows (List.length (String.lines editedBodyText) + 1)
            , value editedBodyText
            , onInput NoteBodyChange
            ]
            []
        , button [ class "note-button", onClick CancelNoteEdit ] [ text "Cancel" ]
        , button [ class "note-button", onClick SaveNoteEdit ] [ text "Save" ]
        ]


noteMatchesQuery : SearchQuery -> Note -> Bool
noteMatchesQuery (SearchQuery queryString) note =
    String.contains queryString note.nTitle
        || String.contains queryString note.nBody
