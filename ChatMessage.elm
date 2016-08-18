module ChatMessage exposing (ChatMessage, fromJson, toJson, typeToString, newChatMessage, newErrorMessage)

import Date exposing (Date)
import Date.Extra
import Json.Decode exposing ((:=))
import Json.Decode as Json
import Json.Encode exposing (object, encode, string)
import I18N


type MessageType
    = Chat
    | Error


type alias ChatMessage =
    { msgType : MessageType
    , name : String
    , text : String
    , time : Date
    }


typeToString : MessageType -> String
typeToString messageType =
    case messageType of
        Chat ->
            "chat"

        Error ->
            "error"


stringToType : String -> Result String MessageType
stringToType str =
    case str of
        "chat" ->
            Ok Chat

        "error" ->
            Ok Error

        _ ->
            Err I18N.unexpectedMessageType


toJson : ChatMessage -> String
toJson message =
    encode 0 (messageEncoder message)


fromJson : String -> Result String ChatMessage
fromJson json =
    Json.decodeString messageDecoder json


messageEncoder : ChatMessage -> Json.Encode.Value
messageEncoder message =
    (object
        [ ( "msgType", string (typeToString message.msgType) )
        , ( "name", string message.name )
        , ( "text", string message.text )
        , ( "time", string (Date.Extra.toUtcIsoString message.time) )
        ]
    )


timeDecoder : Json.Decoder String -> Json.Decoder Date
timeDecoder d =
    Json.customDecoder d Date.fromString


messageTypeDecoder : Json.Decoder String -> Json.Decoder MessageType
messageTypeDecoder d =
    Json.customDecoder d stringToType


messageDecoder : Json.Decoder ChatMessage
messageDecoder =
    Json.object4 ChatMessage
        ("msgType" := Json.string |> messageTypeDecoder)
        ("name" := Json.string)
        ("text" := Json.string)
        ("time" := Json.string |> timeDecoder)


newChatMessage : Date -> String -> String -> ChatMessage
newChatMessage time name text =
    { msgType = Chat
    , name = name
    , text = text
    , time = time
    }


newErrorMessage : Date -> String -> ChatMessage
newErrorMessage time description =
    { msgType = Error
    , name = ""
    , text = description
    , time = time
    }
