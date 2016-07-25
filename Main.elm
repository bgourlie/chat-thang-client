module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import WebSocket
import Json.Decode exposing ((:=))
import Json.Decode as Json


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


echoServer : String
echoServer =
    "ws://localhost:2794"


messageDecoder : Json.Decoder Message
messageDecoder =
    Json.object2 Message
        ("name" := Json.string)
        ("text" := Json.string)


readMessage : String -> Msg
readMessage json =
    case Json.decodeString messageDecoder json of
        Ok message ->
            NewMessage message

        Err message ->
            DecodeError message



-- MODEL


type alias Message =
    { user : String
    , text : String
    }


type alias Model =
    { input : String
    , messages : List Message
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Send
    | NewMessage Message
    | DecodeError String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, messages } =
    case msg of
        Input newInput ->
            ( Model newInput messages, Cmd.none )

        Send ->
            ( Model "" messages, WebSocket.send echoServer input )

        NewMessage msg ->
            ( Model input (msg :: messages), Cmd.none )

        DecodeError string ->
            ( Model input messages, Cmd.none )

        NoOp ->
            ( Model input messages, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen echoServer readMessage
        , listenEnterKey
        ]


listenEnterKey : Sub Msg
listenEnterKey =
    Keyboard.presses
        (\key ->
            if key == 13 then
                Send
            else
                NoOp
        )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input, value model.input ] []
        , button [ onClick Send ] [ text "Send" ]
        , div [] (List.map viewMessage (List.reverse model.messages))
        ]


viewMessage : Message -> Html msg
viewMessage message =
    div [] [ text (message.user ++ ": " ++ message.text) ]
