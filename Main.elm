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
    Json.object3 Message
        ("msgType" := Json.string)
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
    { msgType : String
    , name : String
    , text : String
    }


type alias Model =
    { userName : String
    , input : String
    , messages : List Message
    }


init : ( Model, Cmd Msg )
init =
    ( Model "anonymous_user" "" [], Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Send
    | NewMessage Message
    | DecodeError String
    | NameChange String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { userName, input, messages } =
    case msg of
        Input newInput ->
            ( Model userName newInput messages, Cmd.none )

        Send ->
            -- TODO: Proper JSON serialization
            ( Model userName "" messages, WebSocket.send echoServer ("{\"msgType\": \"chat\", \"name\":\"" ++ userName ++ " \", \"text\":\"" ++ input ++ "\"}") )

        NewMessage msg ->
            ( Model userName input (msg :: messages), Cmd.none )

        DecodeError string ->
            ( Model userName input messages, Cmd.none )

        NameChange newName ->
            ( Model newName input messages, Cmd.none )

        NoOp ->
            ( Model userName input messages, Cmd.none )



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
        [ div []
            [ text ("Your username: " ++ model.userName) ]
        , label []
            [ text "User Name: "
            , input [ onInput NameChange ] []
            ]
        , input [ onInput Input, value model.input ] []
        , button [ onClick Send ] [ text "Send" ]
        , div [] (List.map viewMessage (List.reverse model.messages))
        ]


viewMessage : Message -> Html msg
viewMessage message =
    div [] [ text (message.name ++ ": " ++ message.text ++ "(" ++ message.msgType ++ ")") ]
