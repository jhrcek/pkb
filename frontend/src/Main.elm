module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Dict.Any
import Highlight
import Html exposing (Html)
import Html.Attributes exposing (class, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Markdown
import Note exposing (Note, NoteId, Notes)
import RemoteData exposing (WebData)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        , onUrlChange = UrlChange
        , onUrlRequest = always NoOp
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


type Msg
    = UrlChange Url
    | NotesReceived (WebData Notes)
      -- Note search
    | SetSearchQuery String
    | ClearSearchQuery
      -- Note editing
    | EditNote NoteId
    | NoteBodyChange String
    | CancelNoteEdit
    | SaveNoteEdit
    | NoOp


type
    NoteEditState
    -- Keep original note (to check if changes made) + String representing edit changes
    = NoteEditState Note String


type SearchQuery
    = SearchQuery String


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { notes = RemoteData.Loading
      , searchQuery = SearchQuery ""
      , noteEditState = Nothing
      }
    , Note.getNotes NotesReceived
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
                                Dict.Any.get nId notes
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

        NoOp ->
            ( model, Cmd.none )


saveNote : NoteEditState -> Model -> ( Model, Cmd Msg )
saveNote (NoteEditState origNote newBody) model =
    let
        newNote =
            { origNote | nBody = newBody }

        command =
            if newBody == origNote.nBody then
                Cmd.none

            else
                Note.postNote NotesReceived newNote
    in
    ( { model
        | noteEditState = Nothing
        , notes = RemoteData.map (Dict.Any.insert newNote.nId newNote) model.notes
      }
    , command
    )


setQueryString :
    String
    -> { a | searchQuery : SearchQuery }
    -> { a | searchQuery : SearchQuery }
setQueryString queryString model =
    { model | searchQuery = SearchQuery queryString }


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.notes of
                RemoteData.Loading ->
                    Html.text "Loading notes ..."

                RemoteData.NotAsked ->
                    Html.text "We should never end up in 'NotAsked' state"

                RemoteData.Failure error ->
                    Html.text ("Error loading notes: " ++ httpErrorToString error)

                RemoteData.Success loadedNotes ->
                    viewPage
                        { notes = loadedNotes
                        , searchQuery = model.searchQuery
                        , noteEditState = model.noteEditState
                        }
    in
    { title = "Personal Knowledge Base"
    , body = [ body ]
    }


viewPage : ModelWithNotes -> Html Msg
viewPage model =
    Html.div []
        [ searchBar model.searchQuery
        , viewNotes model
        ]


viewNotes : ModelWithNotes -> Html Msg
viewNotes model =
    model.notes
        |> Dict.Any.values
        |> List.filter (noteMatchesQuery model.searchQuery)
        |> List.sortBy .nTitle
        |> List.map (viewNote model.noteEditState model.searchQuery)
        |> Html.div []


searchBar : SearchQuery -> Html Msg
searchBar (SearchQuery queryString) =
    Html.div []
        [ Html.input [ type_ "text", onInput SetSearchQuery, value queryString ] []
        , Html.button [ onClick ClearSearchQuery ] [ Html.text "Clear" ]
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
            Html.div [ class "note-body" ]
                [ Markdown.toHtml [] n.nBody
                , Html.button [ class "note-button", onClick (EditNote n.nId) ] [ Html.text "Edit" ]
                ]
    in
    Html.details []
        [ Html.summary [ class "note-title" ] [ Highlight.highlight searchQuery note.nTitle ]
        , noteBodyView
        ]


noteBodyEditor : String -> Html Msg
noteBodyEditor editedBodyText =
    Html.div []
        [ Html.textarea
            [ class "note-edit-area"
            , rows (List.length (String.lines editedBodyText) + 1)
            , value editedBodyText
            , onInput NoteBodyChange
            ]
            []
        , Html.button [ class "note-button", onClick CancelNoteEdit ] [ Html.text "Cancel" ]
        , Html.button [ class "note-button", onClick SaveNoteEdit ] [ Html.text "Save" ]
        ]


noteMatchesQuery : SearchQuery -> Note -> Bool
noteMatchesQuery (SearchQuery queryString) note =
    String.contains queryString note.nTitle
        || String.contains queryString note.nBody


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
