module Note exposing
    ( Note
    , NoteId(..)
    , Notes
    , encodeNote
    , getNotes
    , notesDecoder
    , postNote
    )

import Dict.Any exposing (AnyDict)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import RemoteData exposing (WebData)


type alias Note =
    { nId : NoteId
    , nFile : String
    , nTitle : String
    , nBody : String
    }


type alias Notes =
    AnyDict Int NoteId Note


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
