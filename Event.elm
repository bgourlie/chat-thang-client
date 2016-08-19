module Event exposing (Event(ChatEvent, ErrorEvent), fromTransportMessage)

import Date exposing (Date)
import TransportMessage exposing (TransportMessage)
import ApplicationModels exposing (ChatMessage, Error)
import I18N


type Event
    = ChatEvent ChatMessage
    | ErrorEvent Error


fromTransportMessage : TransportMessage -> Date -> Event
fromTransportMessage transportMessage time =
    case transportMessage.msgType of
        "chat" ->
            newChatMessage transportMessage.name transportMessage.text transportMessage.time time

        "error" ->
            newErrorMessage transportMessage.text transportMessage.time time

        _ ->
            newErrorMessage I18N.unexpectedMessageType time time


newChatMessage : String -> String -> Date -> Date -> Event
newChatMessage name text sentTime receivedTime =
    ChatEvent
        { name = name
        , text = text
        , sentTime = sentTime
        , receivedTime = receivedTime
        }


newErrorMessage : String -> Date -> Date -> Event
newErrorMessage description sentTime receivedTime =
    ErrorEvent
        { description = description
        , sentTime = sentTime
        , receivedTime = receivedTime
        }
