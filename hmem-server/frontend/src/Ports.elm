port module Ports exposing (..)

import Json.Encode as Encode



-- PORTS: WebSocket


port connectWebSocket : String -> Cmd msg


port disconnectWebSocket : () -> Cmd msg


port sendWebSocket : String -> Cmd msg


port wsConnected : (() -> msg) -> Sub msg


port wsDisconnected : (() -> msg) -> Sub msg


port wsMessage : (String -> msg) -> Sub msg



-- PORTS: Cytoscape


port initCytoscape : Encode.Value -> Cmd msg


port destroyCytoscape : () -> Cmd msg


port updateCytoscape : Encode.Value -> Cmd msg


port cytoscapeNodeClicked : (String -> msg) -> Sub msg


port cytoscapeEdgeClicked : (String -> msg) -> Sub msg


-- PORTS: Clipboard


port copyToClipboard : String -> Cmd msg


-- PORTS: Local storage


port saveToLocalStorage : Encode.Value -> Cmd msg


port requestLocalStorage : String -> Cmd msg


port localStorageReceived : (Encode.Value -> msg) -> Sub msg



-- PORTS: Scroll


port onMainContentScroll : (Float -> msg) -> Sub msg
