module UpdateRouter exposing (MainOwnedMsg(..), update)

import AppShell
import Browser
import Feature.AuditLog
import Feature.Cards
import Feature.DataLoading
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Groups
import Feature.Mutations
import Feature.Observation
import Feature.Search
import Feature.Timeline
import Feature.WebSocket
import Feature.WorkspaceAdmin
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
        -- WebSocket
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

        CanonicalProjectOverviewFetched _ _ _ ->
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

        CanonicalSessionFetched _ _ _ ->
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

        -- HTTP responses
        GotWorkspaces _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotWorkspace _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotRootNavigation _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotNavigationBranch _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotNavigationFocus _ _ _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotSessionContext epoch expectedWorkspace result ->
            Err (HandleInAppShell (AppShell.SessionContextLoadedMsg epoch expectedWorkspace result))

        GotProjects _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotTasks _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotObservations _ _ _ _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

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

        SetTimelineHistogramBucket _ ->
            Ok (Feature.Timeline.update msg model)

        SelectTimelineHistogramBucket _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        ResetTimelineHistogramSelection ->
            Ok (Feature.Timeline.update msg model)

        ToggleTimelineChartSeries _ ->
            Ok (Feature.Timeline.update msg model)

        FocusTimelineChartPoint _ _ _ ->
            Ok (Feature.Timeline.update msg model)

        -- Mutation responses
        MutationDone _ _ ->
            Ok (Feature.Mutations.update msg model)

        ProjectCreated _ ->
            Ok (Feature.Mutations.update msg model)

        TaskCreated _ ->
            Ok (Feature.Mutations.update msg model)

        MemoryCreated _ ->
            Ok (Feature.Mutations.update msg model)

        ProjectUpdated _ ->
            Ok (Feature.Mutations.update msg model)

        TaskUpdated _ ->
            Ok (Feature.Mutations.update msg model)

        MemoryUpdated _ ->
            Ok (Feature.Mutations.update msg model)

        WorkspaceUpdated _ _ ->
            Ok (Feature.Mutations.update msg model)

        WorkspaceCreated _ ->
            Ok (Feature.Mutations.update msg model)

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

        SubmitSearch ->
            Ok (Feature.Search.update msg model)

        GotUnifiedSearchResults _ _ _ _ ->
            Ok (Feature.Search.update msg model)

        NavigateToSearchResult _ _ ->
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

        SetObservationQuery _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationSubjectKind _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationSubject _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationGitSha _ ->
            Ok (Feature.Observation.update msg model)

        ApplyObservationFilters ->
            Ok (Feature.Observation.update msg model)

        SetObservationBrowseMode _ ->
            Ok (Feature.Observation.update msg model)

        SelectObservationFacet _ _ ->
            Ok (Feature.Observation.update msg model)

        SetObservationMatchPaths _ ->
            Ok (Feature.Observation.update msg model)

        ApplyObservationMatch ->
            Ok (Feature.Observation.update msg model)

        ClearObservationMatch ->
            Ok (Feature.Observation.update msg model)

        LoadMoreObservations ->
            Ok (Feature.Observation.update msg model)

        LoadMoreObservationFacets ->
            Ok (Feature.Observation.update msg model)

        ToggleObservationMatchGroup _ ->
            Ok (Feature.Observation.update msg model)

        SelectObservation _ ->
            Ok (Feature.Observation.update msg model)

        CopyObservationSubject _ ->
            Ok (Feature.Observation.update msg model)

        StartObservationEdit ->
            Ok (Feature.Observation.update msg model)

        SetObservationDraft _ ->
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

        CopyId _ ->
            Ok (Feature.Cards.update msg model)

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
        GotTaskDependencies _ _ ->
            Ok (Feature.Dependencies.update msg model)

        GotTaskDependencyPage _ _ _ _ _ _ ->
            Ok (Feature.Dependencies.update msg model)

        LoadTaskDependencyPage _ ->
            Ok (Feature.Dependencies.update msg model)

        GotProjectOverview _ _ ->
            Ok (Feature.Dependencies.update msg model)

        GotProjectNextTasks _ _ ->
            Ok (Feature.Cards.update msg model)

        GotProjectNextTaskDiagnostics _ _ ->
            Ok (Feature.Cards.update msg model)

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

        DependencyMutationDone _ _ ->
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
