module Requests exposing (getNotes, postNote)

import Http exposing (expectJson)
import Note exposing (Note)
import RemoteData
import Types exposing (Msg(..))


getNotes : Cmd Msg
getNotes =
    Http.get
        { url = "/notes"
        , expect = expectJson (NotesReceived << RemoteData.fromResult) Note.notesDecoder
        }


postNote : Note -> Cmd Msg
postNote note =
    Http.post
        { url = "/notes"
        , body = Http.jsonBody <| Note.encodeNote note
        , expect = expectJson (NotesReceived << RemoteData.fromResult) Note.notesDecoder
        }
