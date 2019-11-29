module Notes.Mock exposing (getNotes)

import Notes exposing (Note, NoteId(..), Notes)
import RemoteData exposing (RemoteData(..), WebData)
import Task


getNotes : (WebData Notes -> msg) -> Cmd msg
getNotes toMsg =
    Task.perform toMsg <| Task.succeed <| RemoteData.Success mockNotes


mockNotes : Notes
mockNotes =
    Notes.fromList
        [ { nId = NoteId 1, nFile = "one.md", nTitle = "note title 1", nBody = "note body 1" }
        , { nId = NoteId 2, nFile = "one.md", nTitle = "note title 2", nBody = "note body 2" }
        ]
