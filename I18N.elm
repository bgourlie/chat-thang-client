module I18N exposing (unexpectedServerError, messageDecodeError, unexpectedMessageType)

import String


unexpectedServerError =
    "An unexpected server error occurred."


messageDecodeError =
    "An error occurred while decoding a message from the server.  You may need to update the client."



-- TODO: Figure out the best way to do String.replace in elm and interpolate unexpected message type into the string


unexpectedMessageType =
    "Encountered an unexpected message type."
