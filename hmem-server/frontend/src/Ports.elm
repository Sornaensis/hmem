port module Ports exposing (..)

import Json.Encode as Encode


-- Presentation-only browser timezone formatting.
port formatTimestamps : Encode.Value -> Cmd msg


port timestampsFormatted : (Encode.Value -> msg) -> Sub msg



-- PORTS: WebSocket


port connectWebSocket : Encode.Value -> Cmd msg


port disconnectWebSocket : () -> Cmd msg


port disconnectChangeStreamScope : Encode.Value -> Cmd msg


port clearChangeStreamScope : Encode.Value -> Cmd msg


port sendWebSocket : String -> Cmd msg


port wsConnected : (() -> msg) -> Sub msg


port wsConnecting : (() -> msg) -> Sub msg


port wsDisconnected : (() -> msg) -> Sub msg


port wsConnectionFailed : (String -> msg) -> Sub msg


port wsMessage : (String -> msg) -> Sub msg


port authUnauthorized : (() -> msg) -> Sub msg


port authTokenChanged : (Bool -> msg) -> Sub msg


port authSessionError : (String -> msg) -> Sub msg


port logoutAuth : () -> Cmd msg


port loginAuth : String -> Cmd msg



-- PORTS: Clipboard


port copyToClipboard : String -> Cmd msg


port clipboardResult : (Bool -> msg) -> Sub msg



-- PORTS: Local storage


port saveToLocalStorage : Encode.Value -> Cmd msg


port requestLocalStorage : String -> Cmd msg


port localStorageReceived : (Encode.Value -> msg) -> Sub msg


port observationPreferenceCommand : Encode.Value -> Cmd msg


port observationPreferencesReceived : (Encode.Value -> msg) -> Sub msg



-- PORTS: Scroll


port onMainContentScroll : (Float -> msg) -> Sub msg


port onHierarchyViewport : (Encode.Value -> msg) -> Sub msg


port syncHierarchyViewport : Encode.Value -> Cmd msg


port scrollHierarchyTarget : String -> Cmd msg


port navigateObservationDetail : Encode.Value -> Cmd msg


port focusObservationToolbar : Encode.Value -> Cmd msg


port onObservationViewport : (Encode.Value -> msg) -> Sub msg


port syncObservationViewport : Encode.Value -> Cmd msg


port searchNavigationCommand : Encode.Value -> Cmd msg


port searchNavigationEvent : (Encode.Value -> msg) -> Sub msg


port observationSearchRuntime : Encode.Value -> Cmd msg


port observationSearchRuntimeReady : (Encode.Value -> msg) -> Sub msg


port resetObservationSearchViewport : Encode.Value -> Cmd msg


port sessionBridgeCommand : Encode.Value -> Cmd msg


port sessionBridgeEvent : (Encode.Value -> msg) -> Sub msg
