module TaskDocumentsTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Documents as Documents
import Feature.DataLoading as DataLoading
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
