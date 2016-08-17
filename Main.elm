module Main exposing (..)

import Html exposing (Html, div, text, input, button, label)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import WebSocket
import Json.Decode exposing ((:=))
import Json.Decode as Json
import Json.Encode exposing (object, encode, string)
import Date exposing (Date, now)
import Date
import Date.Extra
import Task
import String exposing (isEmpty)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


echoServer : String
echoServer =
    "ws://localhost:8080"


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


readMessage : String -> AppMessage
readMessage json =
    case Json.decodeString messageDecoder json of
        Ok message ->
            NewMessage message

        Err message ->
            -- If the server fails to serialize a message, it returns an empty object.
            if json == "{}" then
                GetTimeAndThen (\time -> Send (newErrorMessage time "An unexpected server error occurred."))
            else
                GetTimeAndThen (\time -> Send (newErrorMessage time "An error occurred while decoding a message from the server.  You may need to update the client."))



-- MODEL


type alias ChatMessage =
    { msgType : String
    , name : String
    , text : String
    , time : Date
    }


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
            ( { model | lastMessageSendTime = chatMessage.time, input = "" }, WebSocket.send echoServer (encode 0 (messageEncoder chatMessage)) )

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
        [ WebSocket.listen echoServer readMessage
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
