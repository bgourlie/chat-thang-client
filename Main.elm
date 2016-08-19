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
import Event
import Event exposing (Event(ChatEvent, ErrorEvent))
import TransportMessage exposing (TransportMessage, toJson)
import I18N
import ApplicationModels exposing (ChatMessage, Error)


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
    GetTimeAndThen
        (\time ->
            case TransportMessage.read json time of
                Ok message ->
                    NewEvent (Event.fromTransportMessage message time)

                Err error ->
                    NewEvent (ErrorEvent error)
        )



-- MODEL


type alias Model =
    { userName : String
    , input : String
    , messages : List MessageViewModel
    }


type MessageViewModel
    = ChatView ChatMessage
    | ErrorView Error


init : ( Model, Cmd AppMessage )
init =
    ( Model "anonymous_user" "" [], Cmd.none )



-- UPDATE


type AppMessage
    = Input String
    | Send TransportMessage
    | GetTimeAndThen (Date -> AppMessage)
    | NewEvent Event
    | NameChange String
    | NoOp


update : AppMessage -> Model -> ( Model, Cmd AppMessage )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send chatMessage ->
            ( { model | input = "" }, transmitChatMessage chatMessage )

        NewEvent event ->
            handleEvent model event

        GetTimeAndThen successHandler ->
            ( model, (Task.perform assertNeverHandler successHandler Date.now) )

        NameChange newName ->
            ( { model | userName = newName }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleEvent : Model -> Event -> ( Model, Cmd AppMessage )
handleEvent model e =
    case e of
        ChatEvent messageData ->
            ( { model | messages = (ChatView messageData :: model.messages) }, Cmd.none )

        ErrorEvent error ->
            ( { model | messages = (ErrorView error :: model.messages) }, Cmd.none )


transmitChatMessage : TransportMessage -> Cmd msg
transmitChatMessage chatMessage =
    WebSocket.send eventServer (TransportMessage.toJson chatMessage)


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
        , input [ placeholder I18N.inputPlaceholder, onInput Input, value model.input ] []
        , button [ onClick (sendMessage model) ] [ text "Send" ]
        ]


viewMessage : MessageViewModel -> Html msg
viewMessage message =
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


sendMessage : Model -> AppMessage
sendMessage model =
    if isEmpty <| String.trim <| model.input then
        NoOp
    else
        GetTimeAndThen (\time -> Send ({ msgType = "chat", name = model.userName, text = model.input, time = time }))



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
