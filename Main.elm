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


readMessage : String -> Msg
readMessage json =
    case Json.decodeString messageDecoder json of
        Ok message ->
            NewMessage message

        Err message ->
            DecodeError message



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


init : ( Model, Cmd Msg )
init =
    ( Model "anonymous_user" "" [] (Date.fromTime 0), Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Send Date
    | GetMessageSendTime
    | NewMessage ChatMessage
    | DecodeError String
    | NameChange String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send time ->
            let
                chatMessage =
                    newChatMessage time model
            in
                ( { model | lastMessageSendTime = time, input = "" }, WebSocket.send echoServer (encode 0 (messageEncoder chatMessage)) )

        NewMessage msg ->
            ( { model | messages = (msg :: model.messages) }, Cmd.none )

        DecodeError string ->
            ( model, Cmd.none )

        GetMessageSendTime ->
            ( model, (Task.perform (\_ -> Debug.crash "") Send Date.now) )

        -- TODO: How should we surface errors like this?
        NameChange newName ->
            ( { model | userName = newName }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen echoServer readMessage
        , listenEnterKey model
        ]


listenEnterKey : Model -> Sub Msg
listenEnterKey model =
    Keyboard.presses
        (\key ->
            if key == 13 then
                GetMessageSendTime
            else
                NoOp
        )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text ("Your username: " ++ model.userName) ]
        , label []
            [ text "User Name: "
            , input [ onInput NameChange ] []
            ]
        , input [ onInput Input, value model.input ] []
        , button [ onClick GetMessageSendTime ] [ text "Send" ]
        , div [] (List.map viewMessage (List.reverse model.messages))
        ]


viewMessage : ChatMessage -> Html msg
viewMessage message =
    div [ class message.msgType ]
        [ div [ style [ inlineBlock, bold, withWidth "150px" ] ] [ text message.name ]
        , div [ style [ inlineBlock ] ] [ text message.text ]
        ]


newChatMessage : Date -> Model -> ChatMessage
newChatMessage time model =
    { msgType = "chat"
    , name = model.userName
    , text = model.input
    , time = time
    }



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
