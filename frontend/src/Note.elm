module Note exposing (Note, NoteId(NoteId), notesDecoder)

import EveryDict exposing (EveryDict)
import Json.Decode as Decode exposing (Decoder)


type alias Note =
    { nId : NoteId
    , nTitle : String
    , nBody : String
    }


type NoteId
    = NoteId Int


notesDecoder : Decoder (EveryDict NoteId Note)
notesDecoder =
    Decode.list noteDecoder
        |> Decode.map EveryDict.fromList


noteDecoder : Decoder ( NoteId, Note )
noteDecoder =
    Decode.map3 Note
        (Decode.map NoteId (Decode.field "nId" Decode.int))
        (Decode.field "nTitle" Decode.string)
        (Decode.field "nBody" Decode.string)
        |> Decode.map (\note -> ( note.nId, note ))
