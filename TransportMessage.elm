module TransportMessage exposing (TransportMessage, read, toJson)

import Date exposing (Date)
import Json.Decode exposing (Decoder)
import Json.Decode as Json
import Json.Encode exposing (object, encode, string)
import Json.Decode exposing ((:=))
import Date.Extra
import I18N
import ApplicationModels exposing (Error)


type alias TransportMessage =
    { msgType : String
    , name : String
    , text : String
    , time : Date
    }


fromJson : String -> Result String TransportMessage
fromJson json =
    case Json.decodeString transportMessageDecoder json of
        Ok transportMessage ->
            Ok transportMessage

        Err message ->
            Err I18N.messageDecodeError


transportMessageDecoder : Decoder TransportMessage
transportMessageDecoder =
    Json.object4 TransportMessage
        ("msgType" := Json.string)
        ("name" := Json.string)
        ("text" := Json.string)
        ("time" := Json.string |> timeDecoder)


transportMessageEncoder : TransportMessage -> Json.Encode.Value
transportMessageEncoder msg =
    (object
        [ ( "msgType", string msg.msgType )
        , ( "name", string msg.name )
        , ( "text", string msg.text )
        , ( "time", string (Date.Extra.toUtcIsoString msg.time) )
        ]
    )


toJson : TransportMessage -> String
toJson message =
    encode 0 (transportMessageEncoder message)


timeDecoder : Json.Decoder String -> Json.Decoder Date
timeDecoder d =
    Json.customDecoder d Date.fromString


read : String -> Date -> Result Error TransportMessage
read json time =
    case fromJson json of
        Ok message ->
            Ok message

        Err message ->
            let
                description =
                    if json == "{}" then
                        -- When the server encounters an unexpected error while serializing a response, it will return
                        -- an empty json object.  We handle that here and surface an appropriate message.
                        I18N.unexpectedServerError
                    else
                        I18N.messageDecodeError
            in
                Err { description = description, sentTime = time, receivedTime = time }
