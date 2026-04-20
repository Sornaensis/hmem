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
import Feature.Graph
import Feature.Groups
import Feature.Memory
import Feature.Mutations
import Feature.Search
import Feature.WebSocket
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

        WsDisconnectedMsg ->
            Ok (Feature.WebSocket.update msg model)

        WsMessageReceived _ ->
            Ok (Feature.WebSocket.update msg model)

        -- Cytoscape
        CytoscapeNodeClicked _ ->
            Ok (Feature.Graph.update msg model)

        CytoscapeEdgeClicked _ ->
            Ok (Feature.Graph.update msg model)

        -- HTTP responses
        GotWorkspaces _ ->
            Ok (Feature.DataLoading.update msg model)

        GotProjects _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotTasks _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotMemories _ _ _ ->
            Ok (Feature.DataLoading.update msg model)

        GotSingleMemory _ ->
            Ok (Feature.DataLoading.update msg model)

        GotVisualization _ ->
            Ok (Feature.Graph.update msg model)

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

        WorkspaceUpdated _ ->
            Ok (Feature.Mutations.update msg model)

        -- UI
        DismissToast _ ->
            Ok (Toast.update msg model)

        AutoDismissToast _ ->
            Ok (Toast.update msg model)

        SearchInput _ ->
            Ok (Feature.Search.update msg model)

        SubmitSearch ->
            Ok (Feature.Search.update msg model)

        GotUnifiedSearchResults _ ->
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

        ToggleFilterTag _ ->
            Ok (Feature.Search.update msg model)

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

        -- Tree collapse
        ToggleTreeNode _ ->
            Ok (Feature.Cards.update msg model)

        ExpandAllNodes ->
            Ok (Feature.Cards.update msg model)

        CollapseAllNodes ->
            Ok (Feature.Cards.update msg model)

        -- Delete
        ConfirmDelete _ _ ->
            Ok (Feature.Cards.update msg model)

        PerformDelete ->
            Ok (Feature.Cards.update msg model)

        CancelDelete ->
            Ok (Feature.Cards.update msg model)

        CopyId _ ->
            Ok (Feature.Cards.update msg model)

        LoadGraphForWorkspace _ ->
            Ok (Feature.Graph.update msg model)

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

        -- Memory linking
        StartLinkMemory _ _ ->
            Ok (Feature.Memory.update msg model)

        LinkMemorySearch _ ->
            Ok (Feature.Memory.update msg model)

        CancelLinkMemory ->
            Ok (Feature.Memory.update msg model)

        PerformLinkMemory _ _ _ ->
            Ok (Feature.Memory.update msg model)

        PerformUnlinkMemory _ _ _ ->
            Ok (Feature.Memory.update msg model)

        MemoryLinkDone _ _ ->
            Ok (Feature.Memory.update msg model)

        GotEntityMemories _ _ ->
            Ok (Feature.Memory.update msg model)

        -- Entity linking from memory cards
        StartLinkEntity _ ->
            Ok (Feature.Memory.update msg model)

        LinkEntitySearch _ ->
            Ok (Feature.Memory.update msg model)

        CancelLinkEntity ->
            Ok (Feature.Memory.update msg model)

        PerformLinkEntity _ _ _ ->
            Ok (Feature.Memory.update msg model)

        PerformUnlinkEntity _ _ _ ->
            Ok (Feature.Memory.update msg model)

        -- Task dependencies
        GotTaskDependencies _ _ ->
            Ok (Feature.Dependencies.update msg model)

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

        FocusBreadcrumbNav _ ->
            Ok (Feature.Focus.update msg model)

        ClearFocus ->
            Ok (Feature.Focus.update msg model)

        GotAuditLog _ ->
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
