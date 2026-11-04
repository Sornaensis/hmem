module TaskDocumentsTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Documents as Documents
import Feature.DataLoading as DataLoading
import Feature.Focus as Focus
import Http
import Set
import Test exposing (Test, describe, test)
import Types exposing (..)
import Url


workspace : String
workspace =
    "workspace"


fixture : Model
fixture =
    let
        url = Url.fromString "http://localhost/workspace/workspace" |> Maybe.withDefault { protocol = Url.Http, host = "localhost", port_ = Nothing, path = "/", query = Nothing, fragment = Nothing }
        flags = { apiUrl = "http://localhost", wsUrl = "ws://localhost", sessionId = "runtime", runtimeMode = "local", authTokenStorageKey = "", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        base = AppShell.initModel Nothing url (WorkspacePage workspace) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        cards = base.cards
        loading = base.dataLoading
        session =
            { authMode = "local", effectiveActor = Nothing
            , principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }
            , globalPermissions = { createWorkspace = False, superadmin = False }
            , workspace = Just { workspaceId = workspace, role = Just "owner", canRead = True, canEdit = True, canAdmin = True }
            }
        task =
            { id = "task", workspaceId = workspace, projectId = Nothing, parentId = Nothing, title = "Task"
            , description = Nothing, status = Api.Done, priority = 1, completedAt = Nothing
            , dependencyCount = 0, memoryLinkCount = 0, createdAt = "", updatedAt = ""
            }
    in
    { base
        | auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just session
        , selectedWorkspaceId = Just workspace, activeTab = ProjectsTab
        , tasks = Dict.singleton task.id task
        , cards = { cards | expandedCards = Dict.singleton task.id True }
        , dataLoading = { loading | visibleDetailDemand = Just ( Set.empty, Set.singleton task.id ) }
    }
        |> (\model -> Documents.reconcile ( model, Cmd.none ) |> Tuple.first)


counts : DocumentReadValue
counts =
    DocumentCountsValue { sourceCount = 1, documentEvidenceCount = 3, agentSessionEvidenceCount = 27, evidenceTotal = 30 }


focusedFixture : Model
focusedFixture =
    let
        cards = fixture.cards
        viewport = cards.viewport
        focus = fixture.focus
        loading = fixture.dataLoading
        row =
            { key = "task:task", kind = "task", entityId = "task", depth = 0
            , parentKind = "workspace_root", parentId = Nothing, zone = Nothing
            }
    in
    { fixture
        | documents = Documents.init
        , focus = { focus | focusedEntity = Just ( "task", "task" ) }
        , dataLoading = { loading | visibleDetailDemand = Nothing }
        , cards = { cards | viewport = { viewport
            | workspaceId = Just workspace, sessionEpoch = fixture.sessionRequestEpoch
            , generation = loading.navigationGeneration, rows = Dict.singleton row.key row
            } }
    }
        |> (\model -> Documents.reconcile ( model, Cmd.none ) |> Tuple.first)


suite : Test
suite =
    describe "task document reads"
        [ test "canonical count is separate from source and both evidence sections" <| \_ ->
            let
                request = fixture.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead ( "task", "counts" ) ( fixture, [] )
                ( settled, _ ) = Documents.update (DocumentReadFinished fixture.documents.scope ( "task", "counts" ) request (Ok counts)) admitted
            in
            Expect.equal
                { counts = [ Just 1, Just 3, Just 27, Just 30 ], occupancy = 0 }
                { counts = List.map (\kind -> Documents.count kind settled "task") [ "source", "evidence", "sessions", "total" ]
                , occupancy = Set.size settled.dataLoading.cardDetailAdmissions }
        , test "losing read authority retires focused reads but the completion still releases its slot" <| \_ ->
            let
                key = ( "task", "counts" )
                request = focusedFixture.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead key ( focusedFixture, [] )
                noReadAuthority =
                    { admitted | sessionContext = Maybe.map (\session -> { session | workspace = Nothing }) admitted.sessionContext }
                ( retired, _ ) = Documents.reconcile ( noReadAuthority, Cmd.none )
                ( final, _ ) = Documents.update (DocumentReadFinished admitted.documents.scope key request (Ok counts)) retired
            in
            Expect.equal
                { before = [ key ], demand = Set.empty, held = True, released = True, value = Nothing }
                { before = Documents.pending focusedFixture, demand = retired.documents.demand
                , held = Set.member request retired.dataLoading.cardDetailAdmissions
                , released = not (Set.member request final.dataLoading.cardDetailAdmissions)
                , value = Documents.count "total" final "task"
                }
        , test "a cached Audit focus only demands documents while its destination is verified" <| \_ ->
            let
                key = ( "task", "counts" )
                focus = focusedFixture.focus
                entry =
                    { id = "audit-event", workspaceId = Just workspace, entityType = "task", entityId = "task"
                    , action = Api.AuditUpdate, oldValues = Nothing, newValues = Nothing, requestId = Nothing
                    , actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing, changedAt = ""
                    }
                context = Focus.auditReturnContext ReturnFromWorkspaceAudit workspace focusedFixture.auditLog.filters False entry ( "task", "task" )
                audit = { focusedFixture | focus = { focus | returnContext = Just context } }
                destination =
                    { workspaceId = workspace, sessionEpoch = audit.sessionRequestEpoch, generation = audit.dataLoading.navigationGeneration
                    , filterFingerprint = DataLoading.navigationFilterFingerprint audit, entityType = "task", entityId = "task"
                    , ancestorOffset = 0, summaryRevision = 0, inFlight = True, succeeded = False
                    }
                reconcile model = Documents.reconcile ( model, Cmd.none ) |> Tuple.first
                withDestination state model =
                    let loading = model.dataLoading in
                    reconcile { model | dataLoading = { loading | navigationFocuses = Dict.singleton "task:task" state } }
                pending = withDestination destination audit
                verified = withDestination { destination | inFlight = False, succeeded = True } pending
                request = verified.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead key ( verified, [] )
                unavailable = withDestination { destination | inFlight = False } admitted
                ( final, _ ) = Documents.update (DocumentReadFinished admitted.documents.scope key request (Ok counts)) unavailable
            in
            Expect.equal
                { demand = [ [], [], [ key ], [] ], held = True, released = True, value = Nothing }
                { demand = List.map Documents.pending [ reconcile audit, pending, verified, unavailable ]
                , held = Set.member request unavailable.dataLoading.cardDetailAdmissions
                , released = not (Set.member request final.dataLoading.cardDetailAdmissions)
                , value = Documents.count "total" final "task"
                }
        , test "old workspace or epoch releases the original physical slot without accepting its data" <| \_ ->
            let
                request = fixture.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead ( "task", "counts" ) ( fixture, [] )
                ( retired, _ ) = Documents.reconcile ( { admitted | sessionRequestEpoch = admitted.sessionRequestEpoch + 1 }, Cmd.none )
                ( final, _ ) = Documents.update (DocumentReadFinished admitted.documents.scope ( "task", "counts" ) request (Ok counts)) retired
            in
            Expect.equal ( True, False, Nothing )
                ( Set.member request retired.dataLoading.cardDetailAdmissions, Set.member request final.dataLoading.cardDetailAdmissions, Documents.count "total" final "task" )
        , test "invalidation during one admitted read waits for physical completion and discards its value" <| \_ ->
            let
                request = fixture.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead ( "task", "counts" ) ( fixture, [] )
                invalid = Documents.invalidate True (Set.singleton "task") admitted
                ( final, _ ) = Documents.update (DocumentReadFinished admitted.documents.scope ( "task", "counts" ) request (Err Http.Timeout)) invalid
            in
            Expect.equal ( [], [ ( "task", "counts" ) ], Nothing )
                ( Documents.pending invalid, Documents.pending final, Documents.count "total" final "task" )
        , test "shared detail admission rejects a seventh physical read" <| \_ ->
            let
                loading = fixture.dataLoading
                full = { fixture | dataLoading = { loading | cardDetailAdmissions = Set.fromList (List.range 1 6) } }
                ( final, _ ) = Documents.beginRead ( "task", "counts" ) ( full, [] )
            in
            Expect.equal ( full.dataLoading.cardDetailAdmissions, full.dataLoading.nextCardDetailRequestId )
                ( final.dataLoading.cardDetailAdmissions, final.dataLoading.nextCardDetailRequestId )
        , test "the final document completion yields a mixed spent wave even with no document demand left" <| \_ ->
            let
                request = fixture.dataLoading.nextCardDetailRequestId
                ( admitted, _ ) = Documents.beginRead ( "task", "counts" ) ( fixture, [] )
                loading = admitted.dataLoading
                permit = loading.backgroundAdmission
                prepared = { admitted | dataLoading = { loading
                    | navigationQueue = [ "task:ordinary" ]
                    , backgroundAdmission = { permit
                        | workspaceId = admitted.selectedWorkspaceId
                        , sessionEpoch = admitted.sessionRequestEpoch
                        , filterFingerprint = DataLoading.navigationFilterFingerprint admitted
                        , acknowledged = True, remaining = 3
                        , branchRequests = Set.singleton "settled-branch", detailRequests = Set.singleton request
                        } } }
                completed = Documents.update (DocumentReadFinished prepared.documents.scope ( "task", "counts" ) request (Ok counts)) prepared
                ( settled, _ ) = DataLoading.settleDocumentWave request prepared completed
            in
            Expect.equal ( [], False, True )
                ( Documents.pending settled, settled.dataLoading.backgroundAdmission.acknowledged, settled.dataLoading.backgroundAdmission.nonce > permit.nonce )
        ]
