module ApplicationModels exposing (ChatMessage, Error)

import Date exposing (Date)


type alias ChatMessage =
    { name : String
    , text : String
    , sentTime : Date
    , receivedTime : Date
    }


type alias Error =
    { description : String
    , sentTime : Date
    , receivedTime : Date
    }
