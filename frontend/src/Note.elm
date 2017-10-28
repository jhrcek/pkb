module Note exposing (Note, notesDecoder)

import Json.Decode as Decode exposing (Decoder)


type alias Note =
    { nId : Int
    , nTitle : String
    , nBody : String
    }


notesDecoder : Decoder (List Note)
notesDecoder =
    Decode.list noteDecoder


noteDecoder : Decoder Note
noteDecoder =
    Decode.map3 Note
        (Decode.field "nId" Decode.int)
        (Decode.field "nTitle" Decode.string)
        (Decode.field "nBody" Decode.string)
