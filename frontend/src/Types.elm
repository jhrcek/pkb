module Types exposing (Msg(..), Notes)

import Hash.Dict exposing (Dict)
import Note exposing (Note, NoteId)
import RemoteData exposing (WebData)
import Url.Parser exposing (Url)


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


type alias Notes =
    Dict NoteId Note
