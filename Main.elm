import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import WebSocket
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



-- MODEL
type alias Message =
  { user: String
  , text: String
  }

type alias Model =
  { input : String
  , messages : List Message
  }


init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)



-- UPDATE


type Msg
  = Input String
  | Send
  | NewMessage Message
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
  case msg of
    Input newInput ->
      (Model newInput messages, Cmd.none)

    Send ->
      (Model "" messages, WebSocket.send echoServer input)

    NewMessage str ->
      (Model input (str :: messages), Cmd.none)

    NoOp ->
      (Model input messages, Cmd.none)


readMessage : String -> Msg
readMessage =


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen echoServer NewMessage,
          listenEnterKey
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
    [ input [onInput Input, value model.input] []
    , button [onClick Send] [text "Send"]
    , div [] (List.map viewMessage (List.reverse model.messages))
    ]


viewMessage : Message -> Html msg
viewMessage msg =
  div [] [ text msg.user ++ (": " ++ msg.text) ]