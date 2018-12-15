module Types exposing (Msg(..), Notes)

import Dict.Any exposing (AnyDict)
import Note exposing (Note, NoteId)
import RemoteData exposing (WebData)
import Url exposing (Url)


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


type alias Notes =
    AnyDict Int NoteId Note
