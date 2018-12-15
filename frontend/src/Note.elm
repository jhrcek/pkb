module Note exposing (Note, NoteId(..), encodeNote, notesDecoder)

import Dict.Any exposing (AnyDict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Note =
    { nId : NoteId
    , nFile : String
    , nTitle : String
    , nBody : String
    }


type NoteId
    = NoteId Int


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
