module Page.Workspace exposing
    ( viewWorkspaceContent
    , viewWorkspaceContext
    , viewWorkspacePage
    , workspaceSummaryParts
    , workspaceTabLabel
    , reconcilePresentation
    )

import Api
import AuditNavigation
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.Editing
import Feature.Observation
import Feature.Search
import Feature.Sessions
import Feature.Timeline
import Feature.WorkspaceAdmin
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon
import Permissions
import Spinner
import TimestampDisplay
import Types exposing (..)


viewWorkspacePage : String -> Model -> Html Msg
viewWorkspacePage wsId model =
    div []
        [ case Dict.get wsId model.workspaces of
            Just ws ->
                if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                    viewReadableWorkspaceContext model ws
                else
                    text ""
            Nothing ->
                text ""
        , viewWorkspaceContent wsId model
        ]


reconcilePresentation : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcilePresentation previous ( current, command ) =
    ( if previous.page /= current.page || previous.selectedWorkspaceId /= current.selectedWorkspaceId || previous.sessionRequestEpoch /= current.sessionRequestEpoch then
        { current | workspaceDetailsExpanded = True }
      else current
    , command )


viewWorkspaceContent : String -> Model -> Html Msg
viewWorkspaceContent wsId model =
    if model.auth.status == AuthReady && not (Permissions.canReadCurrentWorkspace model) then
        viewUnauthorizedWorkspace model

    else
        case AuditNavigation.destinationStatus model of
            Just AuditNavigation.DestinationPending ->
                div [ class "audit-destination-status" ] [ Spinner.block "Loading entity destination" ]

            Just AuditNavigation.DestinationUnavailable ->
                div [ class "empty-state audit-destination-status", attribute "role" "status" ]
                    [ text "Entity destination unavailable. It may no longer exist or be accessible." ]

            Nothing ->
                case Dict.get wsId model.workspaces of
                    Nothing ->
                        div [ class "page" ]
                            [ if model.dataLoading.activeWorkspaceLoadToken /= Nothing || model.dataLoading.loadingWorkspaces then
                                Spinner.block "Loading workspace"

                              else
                                div [ class "empty-state" ] [ text "Workspace unavailable." ]
                            ]

                    Just ws ->
                        viewReadableWorkspacePage wsId model ws


viewWorkspaceContext : Model -> Html Msg
viewWorkspaceContext model =
    case model.page of
        WorkspacePage wsId ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                case Dict.get wsId model.workspaces of
                    Just ws ->
                        viewReadableWorkspaceContext model ws

                    Nothing ->
                        text ""

            else
                text ""

        _ ->
            text ""


viewReadableWorkspaceContext : Model -> Api.Workspace -> Html Msg
viewReadableWorkspaceContext model ws =
    let
        summaryParts =
            workspaceSummaryParts model.dataLoading.planningCounts
    in
    div [class "workspace-context-persistent",attribute "aria-label" "Workspace context"]
          [ viewTabs model
          , div [ class "workspace-header" ]
            [ div [ class "workspace-header-top" ]
                [ div [ class "workspace-header-title" ]
                    [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                        [ text (Api.workspaceTypeToString ws.workspaceType) ]
                    , Feature.Editing.viewEditableText model "workspace" ws.id "name" ws.name
                    ]
                , div [ class "workspace-header-actions" ]
                    [ viewCreateButton model.activeTab
                    , if Permissions.canAdminCurrentWorkspace model then
                        button [ class "btn-small btn-danger-subtle workspace-delete-action", onClick (ConfirmWorkspaceDelete ws.id), disabled (model.groups.workspaceDeletion /= Nothing) ] [ text "Delete workspace" ]
                      else text ""
                    ]
                ]
            , div [ class "workspace-summary" ]
                [ viewPlanningSummary model summaryParts
                , if ws.workspaceType == Api.Repository then
                    viewObservationTotal model
                  else text ""
                , span [ class "workspace-summary-separator", attribute "aria-hidden" "true" ] [ text " · " ]
                , button [ class "workspace-details-toggle", type_ "button", onClick ToggleWorkspaceDetails, attribute "aria-expanded" (if model.workspaceDetailsExpanded then "true" else "false"), attribute "aria-controls" "workspace-metadata" ]
                    [ text "Details", Icon.view (if model.workspaceDetailsExpanded then "chevron-down" else "chevron-right") ]
                ]
            , div [class "workspace-metadata", id "workspace-metadata", hidden (not model.workspaceDetailsExpanded)]
                [ div [ class "workspace-details" ]
                  [ div [ class "workspace-detail" ]
                    [ Helpers.copyableValue "workspace-detail-value card-id card-id-copy" "workspace ID" ws.id ws.id ]
                , case ws.ghOwner of
                    Just owner ->
                        div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "GitHub" ]
                            , span [ class "workspace-detail-value" ]
                                [ text
                                    (owner
                                        ++ (case ws.ghRepo of
                                                Just repo ->
                                                    "/" ++ repo

                                                Nothing ->
                                                    ""
                                           )
                                    )
                                ]
                            ]

                    Nothing ->
                        text ""
                , div [ class "workspace-detail" ]
                    [ span [ class "workspace-detail-label" ] [ text "Created" ]
                    , span [ class "workspace-detail-value" ] [ text (TimestampDisplay.lookup model.timestamps ws.createdAt) ]
                    ]
                , div [ class "workspace-detail" ]
                    [ span [ class "workspace-detail-label" ] [ text "Updated" ]
                    , span [ class "workspace-detail-value" ] [ text (TimestampDisplay.lookup model.timestamps ws.updatedAt) ]
                    ]
                  ]
                ]
            ]

          ]


viewReadableWorkspacePage : String -> Model -> Api.Workspace -> Html Msg
viewReadableWorkspacePage wsId model ws =
    div [ class "page workspace-page" ]
        [ if not (Permissions.canReadCurrentWorkspace model) then
            viewUnauthorizedWorkspace model

          else
            div []
                [ Feature.Search.viewNavigation model
                    (if model.activeTab == AdministrationTab || model.activeTab == SearchTab then
                        viewTabContent wsId model
                     else if model.dataLoading.loadingWorkspaceData then
                        Spinner.block "Loading workspace data"
                     else
                        viewTabContent wsId model)

                ]
        ]


workspaceSummaryParts : PlanningCountState -> List String
workspaceSummaryParts counts =
    if counts.error /= Nothing then
        [ "Planning totals unavailable" ]

    else
        case counts.value of
            Just value ->
                [ String.fromInt value.openProjects ++ " open " ++ (if value.openProjects == 1 then "project" else "projects")
                , String.fromInt value.openTasks ++ " open " ++ (if value.openTasks == 1 then "task" else "tasks")
                ]

            Nothing ->
                if counts.current then [ "Planning totals unavailable" ] else []


viewPlanningSummary : Model -> List String -> Html Msg
viewPlanningSummary model parts =
    let
        counts = model.dataLoading.planningCounts
    in
    span [ class "workspace-planning-total", id "workspace-planning-total", tabindex -1, attribute "role" "group", attribute "aria-label" "Planning totals" ]
        [ span [ attribute "aria-live" "polite" ] [ text (String.join " · " parts) ]
        , if counts.error /= Nothing then
            button [ class "btn btn-sm", type_ "button", Spinner.preserveFocus "workspace-planning-total", onClick RetryWorkspacePlanningCounts ] [ text "Retry planning totals" ]

          else if not counts.current then
            span [ class "workspace-total-pending" ]
                [ Spinner.inline (if counts.value == Nothing then "Loading planning totals" else "Updating planning totals") ]

          else
            text ""
        ]


viewObservationTotal : Model -> Html Msg
viewObservationTotal model =
    let
        counts = model.observations.workspaceCounts
        label = counts.value |> Maybe.map (\value -> String.fromInt value.totalCount ++ " observations")
        sharedPending = not counts.current && counts.error == Nothing && AuditNavigation.destinationStatus model == Nothing && Feature.Observation.workspaceTotalUsesVisibleDiscovery model
    in
    span [ class "workspace-observation-total", id "workspace-observation-total", tabindex -1, attribute "role" "group", attribute "aria-label" "Observation total" ]
        [ text (if sharedPending && label == Nothing then "" else " · ")
        , span [ attribute "aria-live" "polite" ]
            [ text (if counts.error /= Nothing || counts.current then Maybe.withDefault "Observation total unavailable" (if counts.error /= Nothing then Nothing else label) else Maybe.withDefault "" label) ]
        , if counts.error /= Nothing then
            button [ class "btn btn-sm", type_ "button", Spinner.preserveFocus "workspace-observation-total", onClick RetryWorkspaceObservationCounts ] [ text "Retry counts" ]

          else if not counts.current && not sharedPending then
            span [ class "workspace-total-pending" ]
                [ Spinner.inline (if counts.value == Nothing then "Loading Observation total" else "Updating Observation total") ]

          else
            text ""
        ]


viewUnauthorizedWorkspace : Model -> Html Msg
viewUnauthorizedWorkspace model =
    if model.sessionContext == Nothing then
        Spinner.block "Loading workspace permissions"

    else
        div [ class "empty-state" ]
            [ h3 [] [ text "No workspace access" ]
            , p [] [ text "You can see this route, but the current session does not have read permission for this workspace." ]
            ]


viewCreateButton : WorkspaceTab -> Html Msg
viewCreateButton _ =
    text ""


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "tabs workspace-tabs", attribute "aria-label" "Workspace sections" ]
        (List.filterMap identity
            [ Just (viewTab ProjectsTab model (workspaceTabLabel ProjectsTab))
            , Just (viewTab SessionsTab model (workspaceTabLabel SessionsTab))
            , Just (viewTab ObservationsTab model (workspaceTabLabel ObservationsTab))
            , Just (viewTab TimelineTab model (workspaceTabLabel TimelineTab))
            , Just (viewTab SearchTab model (workspaceTabLabel SearchTab))
            , if Permissions.canViewCurrentWorkspaceAudit model then
                Just (viewTab AuditTab model (workspaceTabLabel AuditTab))

              else
                Nothing
            , if Permissions.canViewWorkspaceAdministration model then
                Just (viewTab AdministrationTab model (workspaceTabLabel AdministrationTab))

              else
                Nothing
            ]
        )


workspaceTabLabel : WorkspaceTab -> String
workspaceTabLabel tab =
    case tab of
        ProjectsTab ->
            "Projects"

        ObservationsTab ->
            "Observations"

        TimelineTab ->
            "Timeline"

        SessionsTab ->
            "Sessions"

        SearchTab ->
            "Search"

        AuditTab ->
            "Audit"

        AdministrationTab ->
            "Administration"


viewTab : WorkspaceTab -> Model -> String -> Html Msg
viewTab tab model label =
    let
        icon =
            case tab of
                ProjectsTab -> "folder-kanban"
                SessionsTab -> "terminal"
                ObservationsTab -> "scan-eye"
                TimelineTab -> "history"
                SearchTab -> "search"
                AuditTab -> "shield-check"
                AdministrationTab -> "settings-2"
        count = model.dataLoading.planningCounts
    in
    button
        [ class
            (if tab == model.activeTab then
                "tab active"

             else
                "tab"
            )
        , onClick (SwitchTab tab)
        , type_ "button"
        , attribute "aria-label" label
        , attribute "aria-current" (if tab == model.activeTab then "page" else "false")
        ]
        [ Icon.view icon
        , text label
        , if tab == ProjectsTab && count.current then
            count.value |> Maybe.map (\value -> span [ class "workspace-tab-count", attribute "aria-hidden" "true", title "Open projects" ] [ text (String.fromInt value.openProjects) ]) |> Maybe.withDefault (text "")
          else text ""
        ]


viewTabContent : String -> Model -> Html Msg
viewTabContent wsId model =
    case model.activeTab of
        ProjectsTab ->
            let
                actions =
                    div [ class "tree-toolbar" ]
                        [ div [ class "hierarchy-action-pair" ]
                            [ button [ class "btn-small btn-ghost", type_ "button", attribute "aria-label" "Expand All", onClick ExpandAllNodes ] [ Icon.view "unfold-vertical", text "Expand all" ]
                            , button [ class "btn-small btn-ghost", type_ "button", attribute "aria-label" "Collapse All", onClick CollapseAllNodes ] [ Icon.view "fold-vertical", text "Collapse all" ]
                            ]
                        , Feature.Editing.viewInlineCreateInput model Nothing "project"
                        ]
            in
            div [ class "hierarchy-content-section" ]
                [ Feature.Search.viewWorkspaceSearchBar actions model
                , Feature.Cards.viewProjectsTree wsId model
                ]

        ObservationsTab ->
            case Dict.get wsId model.workspaces of
                Just workspace ->
                    Feature.Observation.viewObservations workspace model

                Nothing ->
                    Spinner.block "Loading observations"

        TimelineTab ->
            Feature.Timeline.viewWorkspaceTimelinePanel wsId model

        SessionsTab ->
            Feature.Sessions.view wsId model

        SearchTab ->
            div [ class "workspace-search-page" ]
                [ h2 [] [ text "Search All Entities" ]
                , Feature.Search.viewSearchBar model
                , if model.search.isSearching then Feature.Search.viewUnifiedSearchLoading model
                  else case model.search.searchError of
                    Just message -> Feature.Search.viewUnifiedSearchError model message
                    Nothing -> case model.search.unifiedResults of
                        Just results -> Feature.Search.viewUnifiedSearchResults model results
                        Nothing -> div [ class "empty-state" ] [ text "Search projects, tasks and observations in this workspace." ]
                ]

        AuditTab ->
            Feature.AuditLog.viewWorkspaceAuditPanel wsId model

        AdministrationTab ->
            case Dict.get wsId model.workspaces of
                Just workspace ->
                    Feature.WorkspaceAdmin.viewWorkspaceAdminPanel workspace model

                Nothing ->
                    Spinner.block "Loading workspace administration"
