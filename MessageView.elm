module MessageView exposing (MessageViewModel(ChatView, ErrorView), messageView)

import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import ApplicationModels exposing (ChatMessage, Error)


type MessageViewModel
    = ChatView ChatMessage
    | ErrorView Error


messageView : MessageViewModel -> Html msg
messageView message =
    case message of
        ChatView messageData ->
            div []
                [ div [ style [ smallFont ] ] [ text (toString messageData.receivedTime) ]
                , div [ style [ inlineBlock, bold, withWidth "150px" ] ] [ text messageData.name ]
                , div [ style [ inlineBlock ] ] [ text messageData.text ]
                ]

        ErrorView error ->
            div []
                [ div [ style [ smallFont ] ] [ text (toString error.receivedTime) ]
                , div [] [ text error.description ]
                ]



-- Styles


inlineBlock : ( String, String )
inlineBlock =
    ( "display", "inline-block" )


bold : ( String, String )
bold =
    ( "font-weight", "bold" )


withWidth : String -> ( String, String )
withWidth width =
    ( "width", width )


smallFont : ( String, String )
smallFont =
    ( "font-size", "10px" )
