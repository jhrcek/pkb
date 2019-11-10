module Notes exposing
    ( Model
    , Msg
    , Note
    , NoteId(..)
    , Notes
    , encodeNote
    , getNotes
    , init
    , notesDecoder
    , postNote
    , update
    , view
    )

import Dict.Any exposing (AnyDict)
import Highlight
import Html exposing (Html)
import Html.Attributes exposing (class, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Markdown
import RemoteData exposing (WebData)


type alias Note =
    { nId : NoteId
    , nFile : String
    , nTitle : String
    , nBody : String
    }


type alias Notes =
    AnyDict Int NoteId Note


type SearchQuery
    = SearchQuery String


type
    NoteEditState
    -- Keep original note (to check if changes made) + String representing edit changes
    = Editing Note String
    | NotEditing


type alias Model =
    { notes : Notes
    , searchQuery : SearchQuery
    , noteEditState : NoteEditState
    }


type Msg
    = SetSearchQuery String
    | ClearSearchQuery
    | EditNote NoteId
    | NoteBodyChange String
    | CancelNoteEdit
    | SaveNoteEdit


type NoteId
    = NoteId Int


init : Notes -> Model
init notes =
    { notes = notes
    , searchQuery = SearchQuery ""
    , noteEditState = NotEditing
    }


noteIdToInt : NoteId -> Int
noteIdToInt (NoteId i) =
    i


notesDecoder : Decoder (AnyDict Int NoteId Note)
notesDecoder =
    Decode.list noteDecoder
        |> Decode.map (Dict.Any.fromList noteIdToInt)


noteDecoder : Decoder ( NoteId, Note )
noteDecoder =
    Decode.map4 Note
        (Decode.map NoteId (Decode.field "nId" Decode.int))
        (Decode.field "nFile" Decode.string)
        (Decode.field "nTitle" Decode.string)
        (Decode.field "nBody" Decode.string)
        |> Decode.map (\note -> ( note.nId, note ))


encodeNote : Note -> Value
encodeNote { nId, nFile, nTitle, nBody } =
    let
        (NoteId noteId) =
            nId
    in
    Encode.object
        [ ( "nId", Encode.int noteId )
        , ( "nFile", Encode.string nFile )
        , ( "nTitle", Encode.string nTitle )
        , ( "nBody", Encode.string nBody )
        ]


getNotes : (WebData Notes -> msg) -> Cmd msg
getNotes toMsg =
    Http.get
        { url = "/notes"
        , expect = Http.expectJson (toMsg << RemoteData.fromResult) notesDecoder
        }


postNote : (WebData Notes -> msg) -> Note -> Cmd msg
postNote toMsg note =
    Http.post
        { url = "/notes"
        , body = Http.jsonBody <| encodeNote note
        , expect = Http.expectJson (toMsg << RemoteData.fromResult) notesDecoder
        }


update : (WebData Notes -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update notesReceived msg model =
    case msg of
        SetSearchQuery queryString ->
            ( setQueryString queryString model
            , Cmd.none
            )

        ClearSearchQuery ->
            ( setQueryString "" model
            , Cmd.none
            )

        EditNote nId ->
            ( { model
                | noteEditState =
                    case Dict.Any.get nId model.notes of
                        Just note ->
                            Editing note note.nBody

                        Nothing ->
                            NotEditing
              }
            , Cmd.none
            )

        NoteBodyChange newText ->
            ( { model
                | noteEditState =
                    case model.noteEditState of
                        NotEditing ->
                            NotEditing

                        Editing note _ ->
                            Editing note newText
              }
            , Cmd.none
            )

        CancelNoteEdit ->
            ( { model | noteEditState = NotEditing }
            , Cmd.none
            )

        SaveNoteEdit ->
            ( model
            , saveNote notesReceived model.noteEditState
            )


saveNote : (WebData Notes -> msg) -> NoteEditState -> Cmd msg
saveNote notesReceived editState =
    case editState of
        Editing origNote newBody ->
            let
                newNote =
                    { origNote | nBody = newBody }
            in
            if newBody /= origNote.nBody then
                postNote notesReceived newNote

            else
                Cmd.none

        NotEditing ->
            Cmd.none


setQueryString :
    String
    -> { a | searchQuery : SearchQuery }
    -> { a | searchQuery : SearchQuery }
setQueryString queryString model =
    { model | searchQuery = SearchQuery queryString }


view : Model -> Html Msg
view model =
    Html.div []
        [ searchBar model.searchQuery
        , notesListView model
        ]


notesListView : Model -> Html Msg
notesListView model =
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


viewNote : NoteEditState -> SearchQuery -> Note -> Html Msg
viewNote editState (SearchQuery searchQuery) note =
    let
        noteBodyView =
            case editState of
                Editing { nId } editedBodyText ->
                    if note.nId == nId then
                        noteBodyEditor editedBodyText

                    else
                        markdownBody note

                NotEditing ->
                    markdownBody note

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
