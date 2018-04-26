module Types exposing (Msg(..), Notes)

import EveryDict exposing (EveryDict)
import Navigation
import Note exposing (Note, NoteId)
import RemoteData exposing (WebData)


type Msg
    = UrlChange Navigation.Location
    | NotesReceived (WebData Notes)
      -- Note search
    | SetSearchQuery String
    | ClearSearchQuery
      -- Note editing
    | EditNote NoteId
    | NoteBodyChange String
    | CancelNoteEdit
    | SaveNoteEdit


type alias Notes =
    EveryDict NoteId Note
