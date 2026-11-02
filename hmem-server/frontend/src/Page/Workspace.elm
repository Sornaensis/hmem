module Page.Workspace exposing
    ( viewWorkspaceContent
    , viewWorkspaceContext
    , viewWorkspacePage
    , workspaceSummaryParts
    , workspaceTabLabel
    , reconcilePresentation
    )

import Api
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.Editing
import Feature.Focus
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

    else if Feature.Focus.auditDestinationStatus model /= Nothing then
        div [ class "empty-state audit-destination-status", attribute "role" "status" ] [ text (Maybe.withDefault "Entity destination unavailable." (Feature.Focus.auditDestinationStatus model)) ]

    else
        case Dict.get wsId model.workspaces of
            Nothing ->
                div [ class "page" ]
                    [ div [ class "loading-indicator" ] [ text "Loading..." ] ]

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
                    let counts = model.observations.workspaceCounts in
                    span [ class "workspace-observation-total", attribute "aria-live" "polite" ]
                        [ text ((if List.isEmpty summaryParts then "" else " · ") ++
                            (if counts.current then counts.value |> Maybe.map (\value -> String.fromInt value.totalCount ++ " observations") |> Maybe.withDefault "Observation total unavailable"
                             else if counts.error /= Nothing then "Observation total unavailable"
                             else if counts.value /= Nothing then "Updating Observation total…" else "Loading Observation total…"))
                        , if counts.error /= Nothing then button [ class "btn btn-sm", type_ "button", onClick RetryWorkspaceObservationCounts ] [ text "Retry counts" ] else text "" ]
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
                        div [ class "loading-indicator" ] [ text "Loading..." ]
                     else
                        viewTabContent wsId model)

                ]
        ]


workspaceSummaryParts : PlanningCountState -> List String
workspaceSummaryParts counts =
    if counts.current then
        case counts.value of
            Just value ->
                [ String.fromInt value.openProjects ++ " open " ++ (if value.openProjects == 1 then "project" else "projects")
                , String.fromInt value.openTasks ++ " open " ++ (if value.openTasks == 1 then "task" else "tasks")
                ]
            Nothing -> [ "Planning totals unavailable" ]
    else if counts.error /= Nothing then
        [ "Planning totals unavailable" ]
    else if counts.value /= Nothing then
        [ "Updating planning totals…" ]
    else
        [ "Loading planning totals…" ]


viewPlanningSummary : Model -> List String -> Html Msg
viewPlanningSummary model parts =
    span [ class "workspace-planning-total", attribute "aria-live" "polite" ]
        [ text (String.join " · " parts)
        , if model.dataLoading.planningCounts.error /= Nothing then
            button [ class "btn btn-sm", type_ "button", onClick RetryWorkspacePlanningCounts ] [ text "Retry planning totals" ]
          else text ""
        ]


viewUnauthorizedWorkspace : Model -> Html Msg
viewUnauthorizedWorkspace model =
    if model.sessionContext == Nothing then
        div [ class "empty-state" ] [ text "Loading workspace permissions..." ]

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
            div [ class "hierarchy-content-section" ]
                [ div [ class "hierarchy-toolbar" ]
                    [ h2 [ class "hierarchy-label" ] [ Icon.view "list-tree", text "Project hierarchy" ]
                    , div [ class "tree-toolbar" ]
                        [ div [ class "hierarchy-action-pair" ]
                            [ button [ class "btn-small btn-ghost", type_ "button", attribute "aria-label" "Expand All", onClick ExpandAllNodes ] [ Icon.view "unfold-vertical", text "Expand all" ]
                            , button [ class "btn-small btn-ghost", type_ "button", attribute "aria-label" "Collapse All", onClick CollapseAllNodes ] [ Icon.view "fold-vertical", text "Collapse all" ]
                            ]
                        , Feature.Editing.viewInlineCreateInput model Nothing "project"
                        ]
                    ]
                , Feature.Search.viewWorkspaceSearchBar model
                , Feature.Cards.viewProjectsTree wsId model
                ]

        ObservationsTab ->
            case Dict.get wsId model.workspaces of
                Just workspace ->
                    Feature.Observation.viewObservations workspace model

                Nothing ->
                    div [ class "loading-indicator" ] [ text "Loading observations..." ]

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
                    div [ class "loading-indicator" ] [ text "Loading workspace administration..." ]
