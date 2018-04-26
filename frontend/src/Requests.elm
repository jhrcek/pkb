module Requests exposing (getNotes, postNote)

import Http exposing (jsonBody)
import Note exposing (Note)
import RemoteData
import Types exposing (Msg(NotesReceived))


getNotes : Cmd Msg
getNotes =
    Http.get "/notes" Note.notesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map NotesReceived


postNote : Note -> Cmd Msg
postNote note =
    Http.post "/notes" (jsonBody (Note.encodeNote note)) Note.notesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map NotesReceived
