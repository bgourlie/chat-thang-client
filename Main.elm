module Main exposing (..)

import Html exposing (Html, div, text, input, button, label)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import WebSocket
import Date exposing (Date, now)
import Task
import String exposing (isEmpty)
import ChatMessage
import ChatMessage exposing (ChatMessage)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


eventServer : String
eventServer =
    "ws://localhost:8080"


readMessage : String -> AppMessage
readMessage json =
    case ChatMessage.fromJson json of
        Ok message ->
            NewMessage message

        Err message ->
            -- If the server fails to serialize a message, it returns an empty object.
            if json == "{}" then
                GetTimeAndThen (\time -> Send (newErrorMessage time "An unexpected server error occurred."))
            else
                GetTimeAndThen (\time -> Send (newErrorMessage time "An error occurred while decoding a message from the server.  You may need to update the client."))



-- MODEL


type alias Model =
    { userName : String
    , input : String
    , messages : List ChatMessage
    , lastMessageSendTime : Date
    }


init : ( Model, Cmd AppMessage )
init =
    ( Model "anonymous_user" "" [] (Date.fromTime 0), Cmd.none )



-- UPDATE


type AppMessage
    = Input String
    | Send ChatMessage
    | GetTimeAndThen (Date -> AppMessage)
    | NewMessage ChatMessage
    | NameChange String
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send chatMessage ->
            ( { model | lastMessageSendTime = chatMessage.time, input = "" }, WebSocket.send eventServer (ChatMessage.toJson chatMessage) )

        NewMessage msg ->
            ( { model | messages = (msg :: model.messages) }, Cmd.none )

        GetTimeAndThen successHandler ->
            ( model, (Task.perform assertNeverHandler successHandler Date.now) )

        NameChange newName ->
            ( { model | userName = newName }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


assertNeverHandler : a -> b
assertNeverHandler =
    (\_ -> Debug.crash "Assertion failed: This should never happen")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub AppMessage
subscriptions model =
    Sub.batch
        [ WebSocket.listen eventServer readMessage
        , listenEnterKey model
        ]


listenEnterKey : Model -> Sub AppMessage
listenEnterKey model =
    Keyboard.presses
        (\key ->
            if key == 13 then
                sendMessage model
            else
                NoOp
        )


updateName : String -> AppMessage
updateName newName =
    if String.length newName <= 16 then
        NameChange newName
    else
        NameChange (String.left 16 newName)



-- VIEW


view : Model -> Html AppMessage
view model =
    div []
        [ div []
            [ text ("Your username: " ++ model.userName) ]
        , label []
            [ text "User Name: "
            , input [ onInput updateName ] []
            ]
        , div [] (List.map viewMessage (List.reverse model.messages))
        , input [ onInput Input, value model.input ] []
        , button [ onClick (sendMessage model) ] [ text "Send" ]
        ]


viewMessage : ChatMessage -> Html msg
viewMessage message =
    div [ class message.msgType ]
        [ div [ style [ smallFont ] ] [ text (toString message.time) ]
        , div [ style [ inlineBlock, bold, withWidth "150px" ] ] [ text message.name ]
        , div [ style [ inlineBlock ] ] [ text message.text ]
        ]


newChatMessage : Date -> Model -> ChatMessage
newChatMessage time model =
    { msgType = "chat"
    , name = model.userName
    , text = model.input
    , time = time
    }


newErrorMessage : Date -> String -> ChatMessage
newErrorMessage time description =
    { msgType = "error"
    , name = "error_reporter"
    , text = description
    , time = time
    }


sendMessage : Model -> AppMessage
sendMessage model =
    if isEmpty <| String.trim <| model.input then
        NoOp
    else
        GetTimeAndThen (\time -> Send (newChatMessage time model))



-- CSS Helpers


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
