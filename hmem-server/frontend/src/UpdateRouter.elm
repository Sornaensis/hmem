module UpdateRouter exposing (MainOwnedMsg(..), reconcileFocusedFilters, update)

import Dict
import Api
import AppShell
import Browser
import Feature.AuditLog
import Feature.AuditDiff
import Feature.Cards
import Feature.DataLoading
import Feature.Dependencies
import Feature.Move
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.FocusedFilters as FocusedFilters
import Feature.Groups
import Feature.Mutations
import Feature.Observation
import Feature.Search
import Feature.SessionActivity
import Feature.Sessions
import Feature.Timeline
import Feature.TimestampDisplay
import Feature.WebSocket
import Feature.WorkspaceAdmin
import Helpers
import Http
import Ports
import Toast
import Types exposing (..)
import Url


type MainOwnedMsg
    = HandleUrlRequest Browser.UrlRequest
    | HandleUrlChange Url.Url
    | HandleInAppShell AppShell.AppShellOwnedMsg


update : Msg -> Model -> Result MainOwnedMsg ( Model, Cmd Msg )
update msg model =
    case msg of
        ObservationViewportChanged payload ->
            Ok (Feature.Observation.updateViewport payload model |> Feature.DataLoading.syncPlanningCountsResult |> Feature.Cards.refreshViewport model)

        HierarchyViewportChanged payload ->
            Ok (Feature.Cards.updateViewport payload model)

        _ ->
            route msg model
                |> Result.map
                    (reconcileFocusedFilters model
                        >> Feature.Cards.prepareViewportFor msg model
                        >> Feature.Observation.refreshHistory
                        >> Feature.Observation.refreshViewport model
                        >> Feature.DataLoading.syncPlanningCountsResult
                        >> Feature.Cards.refreshViewportFor msg model
                    )


reconcileFocusedFilters : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcileFocusedFilters previous ( updated, command ) =
    let
        reconciled = FocusedFilters.reconcile previous updated
        transitioned = ((previous.focusedFilters == Nothing) /= (reconciled.focusedFilters == Nothing)
                || previous.focus.focusedEntity /= reconciled.focus.focusedEntity
                || Maybe.map .entryRevision previous.focusedFilters /= Maybe.map .entryRevision reconciled.focusedFilters)
            && previous.selectedWorkspaceId == reconciled.selectedWorkspaceId
            && previous.sessionRequestEpoch == reconciled.sessionRequestEpoch
    in
    if transitioned then
        let
            filtersChanged = FocusedFilters.capture previous.search /= FocusedFilters.capture reconciled.search
            exiting = reconciled.focusedFilters == Nothing && previous.focusedFilters /= Nothing
            ( reloaded, reloadCommand ) = if filtersChanged || exiting then Feature.DataLoading.revalidateNavigationForFilters reconciled else ( reconciled, Cmd.none )
            ( focused, focusCommand ) = case ( reloaded.selectedWorkspaceId, reloaded.focus.focusedEntity ) of
                ( Just workspaceId, Just ( kind, id ) ) -> Feature.DataLoading.beginNavigationFocus workspaceId kind id reloaded
                _ -> ( reloaded, Cmd.none )
            ( expanded, branchCommand ) =
                if focused.focus.focusedEntity /= Nothing then Feature.DataLoading.ensureExpandedBranches focused else ( focused, Cmd.none )
            ( admitted, admissionCommand ) = Feature.DataLoading.pumpNavigation expanded
        in
        ( admitted, Cmd.batch [ command, reloadCommand, focusCommand, branchCommand, admissionCommand ] )
    else
        ( reconciled, command )


route : Msg -> Model -> Result MainOwnedMsg ( Model, Cmd Msg )
route msg model =
    case msg of
        SessionsNextPage _ -> Ok (Feature.Sessions.update msg model)
        SessionsPreviousPage -> Ok (Feature.Sessions.update msg model)
        SessionsLinkedPage _ -> Ok (Feature.Sessions.update msg model)
        SessionsRetry -> Ok (Feature.Sessions.update msg model)
        SessionsStateFilter _ -> Ok (Feature.Sessions.update msg model)
        SessionsFreshnessFilter _ -> Ok (Feature.Sessions.update msg model)
        SessionsFollow _ -> Ok (Feature.Sessions.update msg model)
        SessionsOlder -> Ok (Feature.Sessions.update msg model)
        SessionsBridgeEvent _ -> Ok (Feature.Sessions.update msg model)
        SessionActivityTick _ -> Ok (Feature.SessionActivity.update msg model)
        SessionActivityVisibility _ -> Ok (Feature.SessionActivity.update msg model)
        SessionActivityFetched _ _ -> Ok (Feature.SessionActivity.update msg model)
        SessionsTick _ -> Ok (Feature.Sessions.update msg model)
        SessionsCancelSave -> Ok (Feature.Sessions.update msg model)
        SessionsTaskNextPage _ _ -> Ok (Feature.Sessions.update msg model)
        SessionsTaskPreviousPage _ -> Ok (Feature.Sessions.update msg model)
        OpenAuditDiff _ _ -> Ok (Feature.AuditDiff.update msg model)
        OpenAuditDiffFrom _ _ _ -> Ok (Feature.AuditDiff.update msg model)
        CloseAuditDiff -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffMode _ -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffBaseline _ -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffField _ -> Ok (Feature.AuditDiff.update msg model)
        RefreshAuditDiff -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffProjectFetched _ _ _ -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffTaskFetched _ _ _ -> Ok (Feature.AuditDiff.update msg model)
        AuditDiffLoaded _ _ _ _ -> Ok (Feature.AuditDiff.update msg model)

        TimestampsFormatted payload ->
            Ok (Feature.TimestampDisplay.receive payload model)

        ObservationViewportChanged payload ->
            Ok (Feature.Observation.updateViewport payload model)

        HierarchyViewportChanged payload ->
            Ok (Feature.Cards.updateViewport payload model)

        -- WebSocket
        DemandSessionResource _ _ ->
            Ok (Feature.WebSocket.update msg model)

        RemoveSessionResource _ _ ->
            Ok (Feature.WebSocket.update msg model)

        SessionResourceFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        RefreshTaskStatusEligibility _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        WsConnectedMsg ->
            Ok (Feature.WebSocket.update msg model)

        WsConnectingMsg ->
            Ok (Feature.WebSocket.update msg model)

        WsDisconnectedMsg ->
            Ok (Feature.WebSocket.update msg model)

        WsConnectionFailed _ ->
            Ok (Feature.WebSocket.update msg model)

        WsMessageReceived _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalWorkspaceFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalProjectFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalTaskFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalObservationFetched _ _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalTaskOverviewFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalReadinessSummariesFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalNavigationSummariesFetched _ _ _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalCatalogueFetched _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalGroupsFetched _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalGroupMembersFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalMembershipsFetched _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        CanonicalSessionFetched _ _ _ _ ->
            Ok (Feature.WebSocket.update msg model)

        AuthUnauthorized ->
            Err (HandleInAppShell AppShell.AuthUnauthorizedMsg)

        AuthTokenChanged present ->
            Err (HandleInAppShell (AppShell.AuthTokenChangedMsg present))

        AuthSessionError message ->
            Err (HandleInAppShell (AppShell.AuthSessionErrorMsg message))

        LoginRequested ->
            Err (HandleInAppShell AppShell.LoginRequestedMsg)

        LogoutRequested ->
            Err (HandleInAppShell AppShell.LogoutRequestedMsg)

        OpenMove _ _ -> Ok (Feature.Move.update msg model)
        CloseMove -> Ok (Feature.Move.update msg model)
        MoveQueryChanged _ -> Ok (Feature.Move.update msg model)
        MoveStatusChanged _ -> Ok (Feature.Move.update msg model)
        MoveToggleBranch _ -> Ok (Feature.Move.update msg model)
        MoveSelect _ -> Ok (Feature.Move.update msg model)
        MoveRows _ -> Ok (Feature.Move.update msg model)
        MoveScroll _ -> Ok (Feature.Move.update msg model)
        MoveFocusRow _ -> Ok (Feature.Move.update msg model)
        MoveRowFocused _ -> Ok (Feature.Move.update msg model)
        MoveRetry _ -> Ok (Feature.Move.update msg model)
        MoveConfirm -> Ok (Feature.Move.update msg model)
        MoveBranchFetched _ _ -> Ok (Feature.Move.update msg model)
        MoveFocusFetched _ _ -> Ok (Feature.Move.update msg model)
        MoveMatchFetched _ _ -> Ok (Feature.Move.update msg model)
        MoveProjectSaved _ _ _ -> Ok (Feature.Move.update msg model)
        MoveTaskSaved _ _ _ _ -> Ok (Feature.Move.update msg model)

        -- HTTP responses
        GotWorkspaces _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotWorkspace _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotWorkspacePlanningCounts _ _ ->
            Ok (Feature.DataLoading.update msg model)

        RetryWorkspacePlanningCounts ->
            Ok (Feature.DataLoading.update msg model)

        GotRootNavigation _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotNavigationBranch _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotNavigationBatch _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotNavigationFocus _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotProjectCardDetail _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotTaskCardDetail _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        RetryCardDetail _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotSessionContext epoch expectedWorkspace result ->
            Err (HandleInAppShell (AppShell.SessionContextLoadedMsg epoch expectedWorkspace result))

        GotProjects _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotTasks _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotObservations _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotWorkspaceObservationCounts _ _ ->
            Ok (Feature.Observation.update msg model)

        RetryWorkspaceObservationCounts ->
            Ok (Feature.Observation.update msg model)

        GotObservationCounts _ _ ->
            Ok (Feature.Observation.update msg model)

        RetryObservationCounts ->
            Ok (Feature.Observation.update msg model)

        GotObservationMatches _ _ _ _ _ _ ->
            Ok (Feature.Observation.update msg model)

        GotObservationSubjectFacets _ _ _ _ _ _ ->
            Ok (Feature.Observation.update msg model)

        GotMemories _ _ _ _ ->
            Ok ( model, Cmd.none )

        GotSingleMemory _ ->
            Ok ( model, Cmd.none )

        GotObservationDetail _ _ _ _ _ ->
            Ok (Feature.Observation.update msg model)

        GotInitialTaskOverview _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotInitialProjectOverview _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotWorkspaceTimeline _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        GotTimelineHistogramClock _ _ ->
            Ok (Feature.Timeline.update msg model)

        GotWorkspaceTimelineBuckets _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        RefreshTimelineAfterDebounce _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        RetryTimelineRefresh ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineEntityFilter _ ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineEventFilter _ ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineHistogramSince _ ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineHistogramUntil _ ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineHistogramWindow _ ->
            Ok (Feature.Timeline.update msg model)

        SetTimelineHistogramBucket _ ->
            Ok (Feature.Timeline.update msg model)

        SelectTimelineHistogramBucket _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        ResetTimelineHistogramSelection ->
            Ok (Feature.Timeline.update msg model)

        ToggleTimelineChartAction _ ->
            Ok (Feature.Timeline.update msg model)

        ToggleTimelineChartSeries _ ->
            Ok (Feature.Timeline.update msg model)

        FocusTimelineChartPoint _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        HoverTimelineOpenBar _ ->
            Ok (Feature.Timeline.update msg model)

        FocusTimelineOpenBar _ ->
            Ok (Feature.Timeline.update msg model)

        HoverTimelineLine _ ->
            Ok (Feature.Timeline.update msg model)

        FocusTimelineLine _ ->
            Ok (Feature.Timeline.update msg model)

        -- Mutation responses
        MutationDone _ _ ->
            Ok (Feature.Mutations.update msg model)

        ProjectCreated _ _ ->
            Ok (Feature.Mutations.update msg model)

        TaskCreated _ _ ->
            Ok (Feature.Mutations.update msg model)

        MemoryCreated _ ->
            Ok (Feature.Mutations.update msg model)

        RequestProjectUpdate workspace epoch projectId fields ->
            Ok (Helpers.beginProjectMutation workspace epoch projectId fields model)

        RequestProjectOverview workspace epoch projectId ->
            Ok (Helpers.beginProjectRequest workspace epoch ("project-overview:" ++ projectId) (\guard -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview guard projectId)) model)

        RequestProjectNextTasks workspace epoch projectId diagnostics ->
            let
                target = (if diagnostics then "project-next-diagnostics:" else "project-next:") ++ projectId
                toMsg guard = GotProjectNextTaskPage guard projectId diagnostics 0 []
            in
            if Helpers.projectNextDemandIsActive projectId model then
                Ok (Helpers.beginProjectRequest workspace epoch target (\guard -> Api.fetchProjectNextTaskPage model.flags.apiUrl projectId 0 diagnostics (toMsg guard)) model)
            else
                let cards = model.cards
                in
                Ok ( { model | cards = { cards | projectNextTasksLoading = Dict.remove projectId cards.projectNextTasksLoading, projectNextTaskDiagnosticsLoading = Dict.remove projectId cards.projectNextTaskDiagnosticsLoading } }, Cmd.none )

        ProjectUpdated guard _ ->
            let
                completed = Helpers.finishProjectOperation guard model
            in
            if Helpers.projectRequestIsCurrent guard model then
                Ok (Feature.Mutations.update msg completed)
            else
                Ok ( completed, Cmd.none )

        RequestTaskUpdate scope taskId requestId fields ->
            Ok (Helpers.beginTaskUpdate scope taskId requestId fields model)

        TaskUpdated _ _ ->
            Ok (Feature.Mutations.update msg model)

        MemoryUpdated _ ->
            Ok (Feature.Mutations.update msg model)

        WorkspaceUpdated _ _ ->
            Ok (Feature.Mutations.update msg model)

        WorkspaceCreated _ ->
            Ok (Feature.Mutations.update msg model)

        ToggleWorkspaceGroup _ ->
            Ok (Feature.Groups.update msg model)

        ConfirmWorkspaceDelete _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        PerformWorkspaceDelete ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        CancelWorkspaceDelete ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspaceDeleteCompleted _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspaceDeleted _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspacePurged _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspaceDeletedForPurge _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        -- UI
        DismissToast _ ->
            Ok (Toast.update msg model)

        AutoDismissToast _ ->
            Ok (Toast.update msg model)

        SearchInput _ ->
            Ok (Feature.Search.update msg model)

        WorkspaceSearch _ ->
            Ok (Feature.Search.update msg model)

        GlobalSearchInput _ ->
            Ok (Feature.Search.update msg model)

        GlobalSearchCompositionStart ->
            Ok (Feature.Search.update msg model)

        GlobalSearchCompositionEnd _ ->
            Ok (Feature.Search.update msg model)

        GlobalSearchReady _ ->
            Ok (Feature.Search.update msg model)

        ToggleSearchEntityType _ ->
            Ok (Feature.Search.update msg model)

        SubmitSearch ->
            Ok (Feature.Search.update msg model)

        GotUnifiedSearchResults _ _ _ _ _ ->
            Ok (Feature.Search.update msg model)

        SearchNavigationEvent _ -> Ok (Feature.Search.update msg model)

        ReturnSearchResults -> Ok (Feature.Search.update msg model)

        NavigateToSearchResult _ _ ->
            Ok (Feature.Search.update msg model)

        SetFilterShowEmptyProjects _ ->
            Ok (Feature.Search.update msg model)

        SetFilterShowOnly _ ->
            Ok (Feature.Search.update msg model)

        SetFilterPriority _ ->
            Ok (Feature.Search.update msg model)

        ToggleFilterProjectStatus _ ->
            Ok (Feature.Search.update msg model)

        ToggleFilterTaskStatus _ ->
            Ok (Feature.Search.update msg model)

        ToggleFilterMemoryType _ ->
            Ok (Feature.Search.update msg model)

        SetFilterImportance _ ->
            Ok (Feature.Search.update msg model)

        SetFilterMemoryPinned _ ->
            Ok (Feature.Search.update msg model)

        ToggleFilterMemoryActiveLinked _ ->
            Ok (Feature.Search.update msg model)

        ToggleFilterTag _ ->
            Ok (Feature.Search.update msg model)

        ObservationDiscoveryRuntimeReceived _ -> Ok (Feature.Observation.update msg model)

        GotObservationDiscovery _ _ -> Ok (Feature.Observation.update msg model)

        SetObservationSubjectQuery _ -> Ok (Feature.Observation.update msg model)
        ClearObservationSearchText _ -> Ok (Feature.Observation.update msg model)

        ObservationFilterCompositionStart -> Ok (Feature.Observation.update msg model)

        ObservationFilterCompositionEnd _ _ -> Ok (Feature.Observation.update msg model)

        SetObservationQuery _ ->
            Ok (Feature.Observation.update msg model)

        ObservationFiltersReady _ ->
            Ok (Feature.Observation.update msg model)

        GotObservationTree _ _ ->
            Ok (Feature.Observation.update msg model)

        GotObservationTreeTerminal _ _ -> Ok (Feature.Observation.update msg model)
        FocusObservationTreePath _ -> Ok (Feature.Observation.update msg model)
        SelectObservationTreePath _ -> Ok (Feature.Observation.update msg model)
        ToggleObservationTreePath _ -> Ok (Feature.Observation.update msg model)
        ObservationTreeKey _ _ -> Ok (Feature.Observation.update msg model)
        LoadObservationTreePage _ _ -> Ok (Feature.Observation.update msg model)

        SetObservationSubjectKind _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationSubject _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationGitSha _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationCurrentGitSha _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationHistoryGitSha _ ->
            Ok (Feature.Observation.update msg model)

        ApplyObservationFilters ->
            Ok (Feature.Observation.update msg model)

        RevertObservationFilters ->
            Ok (Feature.Observation.update msg model)

        RefreshObservationResults ->
            Ok (Feature.Observation.update msg model)

        RetryObservationResults ->
            Ok (Feature.Observation.update msg model)

        RetryObservationDetail ->
            Ok (Feature.Observation.update msg model)

        SetObservationBrowseMode _ ->
            Ok (Feature.Observation.update msg model)

        SelectObservationFacet _ _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationMatchPaths _ ->
            Ok (Feature.Observation.update msg model)

        OpenObservationFileComposer ->
            Ok (Feature.Observation.update msg model)

        CloseObservationFileComposer ->
            Ok (Feature.Observation.update msg model)

        ToggleObservationAdvancedFilters ->
            Ok (Feature.Observation.update msg model)

        ApplyObservationMatch ->
            Ok (Feature.Observation.update msg model)

        ClearObservationMatch ->
            Ok (Feature.Observation.update msg model)

        LoadMoreObservations ->
            Ok (Feature.Observation.update msg model)

        LoadMoreObservationFacets ->
            Ok (Feature.Observation.update msg model)

        ToggleObservationSubjects _ ->
            Ok (Feature.Observation.update msg model)

        ObservationPreferencesReceived _ ->
            Ok (Feature.Observation.update msg model)

        ToggleObservationMatchGroup _ ->
            Ok (Feature.Observation.update msg model)

        SelectObservation _ ->
            Ok (Feature.Observation.update msg model)

        SelectObservationFrom _ _ ->
            Ok (Feature.Observation.update msg model)

        ReturnObservationResults ->
            Ok (Feature.Observation.update msg model)

        CopyObservationSubject _ ->
            Ok (Feature.Observation.update msg model)

        CopyObservationGitSha _ ->
            Ok (Feature.Observation.update msg model)

        SynchronizeWorkspaceFragment ->
            Ok (Helpers.writeObservationHistory False model)

        CopyObservationContent _ ->
            Ok (Feature.Observation.update msg model)

        StartObservationEdit ->
            Ok (Feature.Observation.update msg model)

        ReturnToObservationDraft ->
            Ok (Feature.Observation.update msg model)

        SetObservationDraft _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationReviewedGitSha _ ->
            Ok (Feature.Observation.update msg model)

        LoadObservationHistory ->
            Ok (Feature.Observation.update msg model)

        ToggleObservationHistory ->
            Ok (Feature.Observation.update msg model)

        GotObservationHistory _ _ ->
            Ok (Feature.Observation.update msg model)

        SaveObservationEdit ->
            Ok (Feature.Observation.update msg model)

        CancelObservationEdit ->
            Ok (Feature.Observation.update msg model)

        ReloadObservationEdit ->
            Ok (Feature.Observation.update msg model)

        RebaseObservationEdit ->
            Ok (Feature.Observation.update msg model)

        ObservationUpdated _ _ ->
            Ok (Feature.Observation.update msg model)

        ObservationConflictCanonicalFetched _ _ _ ->
            Ok (Feature.Observation.update msg model)

        OpenObservationDelete ->
            Ok (Feature.Observation.update msg model)

        ConfirmObservationDelete ->
            Ok (Feature.Observation.update msg model)

        CancelObservationDelete ->
            Ok (Feature.Observation.update msg model)

        ObservationDeleteDialogKeyDown _ _ _ ->
            Ok (Feature.Observation.update msg model)

        ObservationDeleted _ _ ->
            Ok (Feature.Observation.update msg model)

        -- Inline editing
        StartEdit _ _ _ _ ->
            Ok (Feature.Editing.update msg model)

        EditInput _ ->
            Ok (Feature.Editing.update msg model)

        SaveEdit _ _ ->
            Ok (Feature.Editing.update msg model)

        CancelEdit ->
            Ok (Feature.Editing.update msg model)

        -- Quick-change handlers
        ChangeProjectStatus _ _ ->
            Ok (Feature.Editing.update msg model)

        ChangeTaskStatus _ _ ->
            Ok (Feature.Editing.update msg model)

        ChangeProjectPriority _ _ ->
            Ok (Feature.Editing.update msg model)

        ChangeTaskPriority _ _ ->
            Ok (Feature.Editing.update msg model)

        ChangeMemoryImportance _ _ ->
            Ok (Feature.Editing.update msg model)

        ToggleMemoryPin _ _ ->
            Ok (Feature.Editing.update msg model)

        ChangeMemoryType _ _ ->
            Ok (Feature.Editing.update msg model)

        -- Tags
        RemoveTag _ _ ->
            Ok (Feature.Editing.update msg model)

        AddTag _ _ ->
            Ok (Feature.Editing.update msg model)

        -- Create forms
        ShowCreateForm _ ->
            Ok (Feature.Editing.update msg model)

        UpdateCreateForm _ ->
            Ok (Feature.Editing.update msg model)

        SubmitCreateForm ->
            Ok (Feature.Editing.update msg model)

        CancelCreateForm ->
            Ok (Feature.Editing.update msg model)

        -- Card expand/collapse
        ToggleCardExpand _ ->
            Ok (Feature.Cards.update msg model)

        LoadNavigationBranchPage _ _ _ ->
            Ok (Feature.Cards.update msg model)

        ShowPreviousNavigationBranchPage _ _ _ ->
            Ok (Feature.Cards.update msg model)

        LoadRootNavigationPage _ ->
            Ok (Feature.DataLoading.update msg model)

        ShowPreviousRootNavigationPage _ ->
            Ok (Feature.DataLoading.update msg model)

        -- Tree collapse
        ToggleTreeNode _ ->
            Ok (Feature.Cards.update msg model)

        ExpandAllNodes ->
            Ok (Feature.Cards.update msg model)

        CollapseAllNodes ->
            Ok (Feature.Cards.update msg model)

        RegisterFocusClick _ _ _ ->
            Ok (Feature.Cards.update msg model)

        -- Delete
        ConfirmDelete _ _ ->
            Ok (Feature.Cards.update msg model)

        PerformDelete ->
            Ok (Feature.Cards.update msg model)

        CascadeDeleteDone _ _ ->
            Ok (Feature.Cards.update msg model)

        CancelDelete ->
            Ok (Feature.Cards.update msg model)

        CopyId value ->
            Ok ( model, Ports.copyToClipboard value )

        ClipboardResult succeeded ->
            Ok (Toast.addToast (if succeeded then Success else Error) (if succeeded then "Copied to clipboard" else "Unable to copy to clipboard") model)

        ExpandAndEdit _ _ _ _ _ ->
            Ok (Feature.Editing.update msg model)

        DragStartCard _ _ ->
            Ok (Feature.DragDrop.update msg model)

        DragOverCard _ ->
            Ok (Feature.DragDrop.update msg model)

        DragOverZone _ ->
            Ok (Feature.DragDrop.update msg model)

        DropOnCard _ _ ->
            Ok (Feature.DragDrop.update msg model)

        DragEndCard ->
            Ok (Feature.DragDrop.update msg model)

        DropOnZone _ ->
            Ok (Feature.DragDrop.update msg model)

        DropActionMakeSubtask ->
            Ok (Feature.DragDrop.update msg model)

        DropActionMakeDependency ->
            Ok (Feature.DragDrop.update msg model)

        CancelDropAction ->
            Ok (Feature.DragDrop.update msg model)

        -- Inline create
        ShowInlineCreate _ ->
            Ok (Feature.Editing.update msg model)

        UpdateInlineCreate _ ->
            Ok (Feature.Editing.update msg model)

        SubmitInlineCreate ->
            Ok (Feature.Editing.update msg model)

        CancelInlineCreate ->
            Ok (Feature.Editing.update msg model)

        -- Legacy memory/link messages are intentionally inert: the Observation-only UI
        -- has no production source that can issue removed memory/link routes.
        StartLinkMemory _ _ ->
            Ok ( model, Cmd.none )

        LinkMemorySearch _ ->
            Ok ( model, Cmd.none )

        CancelLinkMemory ->
            Ok ( model, Cmd.none )

        PerformLinkMemory _ _ _ ->
            Ok ( model, Cmd.none )

        PerformUnlinkMemory _ _ _ ->
            Ok ( model, Cmd.none )

        MemoryLinkDone _ _ ->
            Ok ( model, Cmd.none )

        GotEntityMemories _ _ ->
            Ok ( model, Cmd.none )

        StartLinkEntity _ ->
            Ok ( model, Cmd.none )

        LinkEntitySearch _ ->
            Ok ( model, Cmd.none )

        CancelLinkEntity ->
            Ok ( model, Cmd.none )

        PerformLinkEntity _ _ _ ->
            Ok ( model, Cmd.none )

        PerformUnlinkEntity _ _ _ ->
            Ok ( model, Cmd.none )

        -- Task dependencies
        RequestTaskOverview _ _ _ ->
            Ok (Feature.Dependencies.update msg model)

        GotTaskDependencies _ _ _ _ _ ->
            Ok (Feature.Dependencies.update msg model)

        GotTaskDependencyPage _ _ _ _ _ _ ->
            Ok (Feature.Dependencies.update msg model)

        RefreshTaskDependencies _ ->
            Ok (Feature.Dependencies.update msg model)

        LoadTaskDependencyPage _ ->
            Ok (Feature.Dependencies.update msg model)

        GotProjectOverview guard _ _ ->
            if Helpers.projectRequestIsCurrent guard model then
                Ok (Feature.Dependencies.update msg model)
            else
                Ok ( model, Cmd.none )

        GotProjectNextTaskPage guard projectId diagnostics offset accumulated result ->
            if Helpers.projectRequestIsCurrent guard model && Helpers.projectNextDemandIsActive projectId model then
                let
                    deliver value = if diagnostics then GotProjectNextTaskDiagnostics guard projectId value else GotProjectNextTasks guard projectId value
                in
                case result of
                    Ok page ->
                        if page.hasMore && (List.isEmpty page.items || offset > 2147483647 - List.length page.items) then
                            Ok (Feature.Cards.update (deliver (Err (Http.BadBody "Next-task continuation did not advance"))) model)
                        else if page.hasMore then
                            let nextOffset = offset + List.length page.items
                            in
                            Ok ( model, Api.fetchProjectNextTaskPage model.flags.apiUrl projectId nextOffset diagnostics (GotProjectNextTaskPage guard projectId diagnostics nextOffset (accumulated ++ page.items)) )
                        else
                            Ok (Feature.Cards.update (deliver (Ok (List.foldl (\item values -> if List.any (\value -> value.task.id == item.task.id) values then values else values ++ [ item ]) [] (accumulated ++ page.items)))) model)
                    Err error ->
                        Ok (Feature.Cards.update (deliver (Err error)) model)
            else
                Ok ( model, Cmd.none )

        GotProjectNextTasks guard projectId _ ->
            if Helpers.projectRequestIsCurrent guard model && Helpers.projectNextDemandIsActive projectId model then
                Ok (Feature.Cards.update msg model)
            else
                Ok ( model, Cmd.none )

        GotProjectNextTaskDiagnostics guard projectId _ ->
            if Helpers.projectRequestIsCurrent guard model && Helpers.projectNextDemandIsActive projectId model then
                Ok (Feature.Cards.update msg model)
            else
                Ok ( model, Cmd.none )

        RefreshProjectNextTasks _ ->
            Ok (Feature.Cards.update msg model)

        StartAddDependency _ ->
            Ok (Feature.Dependencies.update msg model)

        DependencySearch _ ->
            Ok (Feature.Dependencies.update msg model)

        CancelAddDependency ->
            Ok (Feature.Dependencies.update msg model)

        PerformAddDependency _ _ ->
            Ok (Feature.Dependencies.update msg model)

        PerformRemoveDependency _ _ ->
            Ok (Feature.Dependencies.update msg model)

        DependencyMutationDone _ _ _ ->
            Ok (Feature.Dependencies.update msg model)

        ScrollToEntity _ ->
            Ok (Feature.Cards.update msg model)

        FocusEntity _ _ ->
            Ok (Feature.Focus.update msg model)

        FocusEntityKeepForward _ _ ->
            Ok (Feature.Focus.update msg model)

        NavigateToAuditEntity _ ->
            Ok (Feature.AuditLog.update msg model)

        NavigateToTimelineEntity _ ->
            Ok (Feature.Timeline.update msg model)

        ReturnToFocusSource ->
            Ok (Feature.Focus.update msg model)

        FocusBreadcrumbNav _ ->
            Ok (Feature.Focus.update msg model)

        ClearFocus ->
            Ok (Feature.Focus.update msg model)

        GotAuditLog _ _ ->
            Ok (Feature.AuditLog.update msg model)

        GotEntityHistory _ _ ->
            Ok (Feature.AuditLog.update msg model)

        ToggleEntityHistory _ _ ->
            Ok (Feature.AuditLog.update msg model)

        LoadMoreHistory _ _ ->
            Ok (Feature.AuditLog.update msg model)

        SetAuditFilter _ _ ->
            Ok (Feature.AuditLog.update msg model)

        ApplyAuditFilters ->
            Ok (Feature.AuditLog.update msg model)

        LoadMoreAuditLog ->
            Ok (Feature.AuditLog.update msg model)

        ToggleAuditExpand _ ->
            Ok (Feature.AuditLog.update msg model)

        ConfirmRevert _ ->
            Ok (Feature.AuditLog.update msg model)

        CancelRevert ->
            Ok (Feature.AuditLog.update msg model)

        PerformRevert ->
            Ok (Feature.AuditLog.update msg model)

        GotRevertResult _ _ _ ->
            Ok (Feature.AuditLog.update msg model)

        ClearPendingMutation _ ->
            Ok (Feature.Mutations.update msg model)

        ClearPendingRequest _ ->
            Ok (Feature.Mutations.update msg model)

        -- Workspace groups
        GotWorkspaceGroups _ ->
            Ok (Feature.Groups.update msg model)

        GotGroupMembers _ _ ->
            Ok (Feature.Groups.update msg model)

        GotWorkspaceMemberships _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        RetryWorkspaceMemberships _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        RetryMembershipAuthorization ->
            Err (HandleInAppShell AppShell.RetryMembershipAuthorizationMsg)

        CreateWorkspaceGroup _ ->
            Ok (Feature.Groups.update msg model)

        WorkspaceGroupCreated _ ->
            Ok (Feature.Groups.update msg model)

        DeleteWorkspaceGroup _ ->
            Ok (Feature.Groups.update msg model)

        WorkspaceGroupDeleted _ _ ->
            Ok (Feature.Groups.update msg model)

        ToggleManageGroup _ ->
            Ok (Feature.Groups.update msg model)

        AddWorkspaceToGroup _ _ ->
            Ok (Feature.Groups.update msg model)

        RemoveWorkspaceFromGroup _ _ ->
            Ok (Feature.Groups.update msg model)

        GroupMembershipDone _ _ ->
            Ok (Feature.Groups.update msg model)

        UpdateMembershipUserId _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        UpdateMembershipRole _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        SubmitWorkspaceMembership _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspaceMembershipSaved _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        RemoveWorkspaceMembership _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        WorkspaceMembershipDeleted _ _ _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        ConfirmWorkspacePurge _ ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        PerformWorkspacePurge ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        CancelWorkspacePurge ->
            Ok (Feature.WorkspaceAdmin.update msg model)

        UrlRequested _ ->
            case msg of
                UrlRequested urlRequest ->
                    Err (HandleUrlRequest urlRequest)

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        UrlChanged _ ->
            case msg of
                UrlChanged url ->
                    Err (HandleUrlChange url)

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        SelectWorkspace _ ->
            case msg of
                SelectWorkspace wsId ->
                    Err (HandleInAppShell (AppShell.SelectWorkspaceMsg wsId))

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        SwitchTab _ ->
            case msg of
                SwitchTab tab ->
                    Err (HandleInAppShell (AppShell.SwitchTabMsg tab))

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        LocalStorageLoaded _ ->
            case msg of
                LocalStorageLoaded json ->
                    Err (HandleInAppShell (AppShell.LocalStorageLoadedMsg json))

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        GlobalKeyDown _ ->
            case msg of
                GlobalKeyDown keyCode ->
                    Err (HandleInAppShell (AppShell.GlobalKeyDownMsg keyCode))

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        MainContentScrolled _ ->
            case msg of
                MainContentScrolled scrollY ->
                    Err (HandleInAppShell (AppShell.MainContentScrolledMsg scrollY))

                _ ->
                    Err (HandleInAppShell AppShell.NoOpMsg)

        NoOp ->
            Err (HandleInAppShell AppShell.NoOpMsg)
