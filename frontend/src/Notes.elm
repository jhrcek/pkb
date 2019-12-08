module Notes exposing
    ( Model
    , Msg
    , Note
    , NoteId(..)
    , Notes
    , encodeNote
    , fromList
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
import Html.Attributes exposing (class, disabled, rows, type_, value)
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
    , editState : NoteEditState
    }


type Msg
    = SearchQueryChanged String
    | SearchQueryCleared
    | NoteEditStarted NoteId
    | NoteBodyInputChanged String
    | NoteEditCanceled
    | NoteEditSaved


type NoteId
    = NoteId Int


init : Notes -> Model
init notes =
    { notes = notes
    , searchQuery = SearchQuery ""
    , editState = NotEditing
    }


noteIdToInt : NoteId -> Int
noteIdToInt (NoteId i) =
    i


fromList : List Note -> Notes
fromList =
    List.map (\note -> ( note.nId, note ))
        >> Dict.Any.fromList noteIdToInt


notesDecoder : Decoder Notes
notesDecoder =
    Decode.list noteDecoder
        |> Decode.map fromList


noteDecoder : Decoder Note
noteDecoder =
    Decode.map4 Note
        (Decode.map NoteId (Decode.field "nId" Decode.int))
        (Decode.field "nFile" Decode.string)
        (Decode.field "nTitle" Decode.string)
        (Decode.field "nBody" Decode.string)


encodeNote : Note -> Value
encodeNote { nId, nFile, nTitle, nBody } =
    Encode.object
        [ ( "nId", Encode.int <| noteIdToInt nId )
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


postNote : (WebData () -> msg) -> Note -> Cmd msg
postNote toMsg note =
    Http.post
        { url = "/notes"
        , body = Http.jsonBody <| encodeNote note
        , expect = Http.expectWhatever (toMsg << RemoteData.fromResult)
        }


update : (WebData () -> msg) -> Msg -> Model -> ( Model, Cmd msg )
update noteSaved msg model =
    case msg of
        SearchQueryChanged queryString ->
            ( setQueryString queryString model
            , Cmd.none
            )

        SearchQueryCleared ->
            ( setQueryString "" model
            , Cmd.none
            )

        NoteEditStarted nId ->
            ( { model
                | editState =
                    case Dict.Any.get nId model.notes of
                        Just note ->
                            Editing note note.nBody

                        Nothing ->
                            NotEditing
              }
            , Cmd.none
            )

        NoteBodyInputChanged newText ->
            ( { model
                | editState =
                    case model.editState of
                        NotEditing ->
                            NotEditing

                        Editing note _ ->
                            Editing note newText
              }
            , Cmd.none
            )

        NoteEditCanceled ->
            ( { model | editState = NotEditing }
            , Cmd.none
            )

        NoteEditSaved ->
            saveNote noteSaved model


saveNote : (WebData () -> msg) -> Model -> ( Model, Cmd msg )
saveNote noteSaved model =
    case model.editState of
        Editing origNote newBody ->
            let
                newNote =
                    { origNote | nBody = newBody }
            in
            ( { model
                | editState = NotEditing
                , notes = Dict.Any.insert newNote.nId newNote model.notes
              }
            , postNote noteSaved newNote
            )

        NotEditing ->
            ( model, Cmd.none )


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
        |> List.map (viewNote model.editState model.searchQuery)
        |> Html.div []


searchBar : SearchQuery -> Html Msg
searchBar (SearchQuery queryString) =
    Html.div []
        [ Html.input [ type_ "text", onInput SearchQueryChanged, value queryString ] []
        , Html.button [ onClick SearchQueryCleared ] [ Html.text "Clear" ]
        ]


viewNote : NoteEditState -> SearchQuery -> Note -> Html Msg
viewNote editState (SearchQuery searchQuery) note =
    let
        noteBodyView =
            case editState of
                Editing editedNote editedBodyText ->
                    if note.nId == editedNote.nId then
                        let
                            contentChanged =
                                editedNote.nBody /= editedBodyText
                        in
                        noteBodyEditor editedBodyText contentChanged

                    else
                        markdownBody note

                NotEditing ->
                    markdownBody note

        markdownBody n =
            Html.div [ class "note-body" ]
                [ Markdown.toHtml [] n.nBody
                , Html.button [ class "note-button", onClick (NoteEditStarted n.nId) ] [ Html.text "Edit" ]
                ]
    in
    Html.details []
        [ Html.summary [ class "note-title" ] [ Highlight.highlight searchQuery note.nTitle ]
        , noteBodyView
        ]


noteBodyEditor : String -> Bool -> Html Msg
noteBodyEditor noteBody contentChanged =
    let
        saveAttribute =
            if contentChanged then
                onClick NoteEditSaved

            else
                disabled True
    in
    Html.div []
        [ Html.textarea
            [ class "note-edit-area"
            , rows (List.length (String.lines noteBody) + 1)
            , value noteBody
            , onInput NoteBodyInputChanged
            ]
            []
        , Html.button [ class "note-button", onClick NoteEditCanceled ] [ Html.text "Cancel" ]
        , Html.button [ class "note-button", saveAttribute ] [ Html.text "Save" ]
        ]


noteMatchesQuery : SearchQuery -> Note -> Bool
noteMatchesQuery (SearchQuery queryString) note =
    String.contains queryString note.nTitle
        || String.contains queryString note.nBody
