module ChatMessage exposing (ChatMessage, fromJson, toJson)

import Date exposing (Date)
import Date.Extra
import Json.Decode exposing ((:=))
import Json.Decode as Json
import Json.Encode exposing (object, encode, string)


type alias ChatMessage =
    { msgType : String
    , name : String
    , text : String
    , time : Date
    }


toJson : ChatMessage -> String
toJson message =
    encode 0 (messageEncoder message)


fromJson : String -> Result String ChatMessage
fromJson json =
    Json.decodeString messageDecoder json


messageEncoder : ChatMessage -> Json.Encode.Value
messageEncoder message =
    (object
        [ ( "msgType", string "chat" )
        , ( "name", string message.name )
        , ( "text", string message.text )
        , ( "time", string (Date.Extra.toUtcIsoString message.time) )
        ]
    )


timeDecoder : Json.Decoder String -> Json.Decoder Date
timeDecoder d =
    Json.customDecoder d Date.fromString


messageDecoder : Json.Decoder ChatMessage
messageDecoder =
    Json.object4 ChatMessage
        ("msgType" := Json.string)
        ("name" := Json.string)
        ("text" := Json.string)
        ("time" := Json.string |> timeDecoder)
