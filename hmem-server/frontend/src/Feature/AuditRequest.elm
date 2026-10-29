module Feature.AuditRequest exposing (append, hasUserConstraints, matches, replace, restore, retire)

import Api
import Dict
import Permissions
import Types exposing (..)


{-| Every audit read, including route/admission/return reads, gets a fresh identity.
The counter survives session retirement; the epoch and audience fence authority.
-}
replace : AuditLogFilters -> Model -> ( Model, Cmd Msg )
replace filters model =
    let
        state = model.auditLog
    in
    restore filters { model | auditLog = { state | expandedEntries = Dict.empty, revertConfirmation = Nothing, revertInFlight = False, restoreScrollY = Nothing, pendingReturn = Nothing } }


restore : AuditLogFilters -> Model -> ( Model, Cmd Msg )
restore =
    start True


append : AuditLogFilters -> Model -> ( Model, Cmd Msg )
append =
    start False


start : Bool -> AuditLogFilters -> Model -> ( Model, Cmd Msg )
start clearEntries filters model =
    let
        state = model.auditLog
        request =
            { requestId = state.nextRequestId
            , sessionEpoch = model.sessionRequestEpoch
            , workspaceId = model.selectedWorkspaceId
            , audience = audience model
            }
    in
    ( { model | auditLog =
        { state
            | entries = if clearEntries then [] else state.entries
            , entryBaseOffset = if clearEntries then Maybe.withDefault 0 filters.offset else state.entryBaseOffset
            , hasMore = if clearEntries then False else state.hasMore
            , filters = filters
            , loading = True
            , loadingFilters = Just filters
            , activeRequest = Just request
            , activeNavigationId = Nothing
            , nextRequestId = state.nextRequestId + 1
            , error = Nothing
        }
      }
    , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog request filters)
    )


matches : AuditRequestIdentity -> AuditLogFilters -> Model -> Bool
matches request filters model =
    model.auth.status == AuthReady
        && model.auditLog.activeRequest == Just request
        && model.auditLog.loadingFilters == Just filters
        && model.auditLog.filters == filters
        && request.sessionEpoch == model.sessionRequestEpoch
        && request.workspaceId == model.selectedWorkspaceId
        && request.audience == audience model
        && (case filters.workspaceId of
                Just workspaceId -> model.page == WorkspacePage workspaceId && model.selectedWorkspaceId == Just workspaceId && Permissions.canViewCurrentWorkspaceAudit model
                Nothing -> model.page == AuditLogPage && Permissions.canViewGlobalAudit model
           )


audience : Model -> Maybe { authMode : String, actorType : String, actorId : String, authority : String, grantUserId : Maybe String }
audience model =
    model.sessionContext
        |> Maybe.map (\session -> { authMode = session.authMode, actorType = session.principal.actorType, actorId = session.principal.actorId, authority = session.principal.authority, grantUserId = session.principal.grantUserId })


retire : AuditLogModel -> AuditLogModel
retire state =
    { state | activeRequest = Nothing, loadingFilters = Nothing, loading = False, error = Nothing }


hasUserConstraints : AuditLogFilters -> Bool
hasUserConstraints filters =
    List.any ((/=) Nothing) [ filters.entityType, filters.entityId, filters.action, filters.since, filters.until ]
