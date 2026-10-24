module Page.Workspace exposing
    ( viewWorkspacePage
    , workspaceSummaryParts
    , workspaceTabLabel
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
import Permissions
import TimestampDisplay
import Types exposing (..)


viewWorkspacePage : String -> Model -> Html Msg
viewWorkspacePage wsId model =
    if model.auth.status == AuthReady && not (Permissions.canReadCurrentWorkspace model) then
        viewUnauthorizedWorkspace model

    else
        case Dict.get wsId model.workspaces of
            Nothing ->
                div [ class "page" ]
                    [ div [ class "loading-indicator" ] [ text "Loading..." ] ]

            Just ws ->
                viewReadableWorkspacePage wsId model ws


viewReadableWorkspacePage : String -> Model -> Api.Workspace -> Html Msg
viewReadableWorkspacePage wsId model ws =
    let
        summaryParts =
            workspaceSummaryParts
                model.dataLoading.planningCounts
    in
    div [ class "page" ]
        [ div [ class "workspace-header" ]
            [ div [ class "workspace-header-top" ]
                [ div [ class "workspace-header-title" ]
                    [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                        [ text (Api.workspaceTypeToString ws.workspaceType) ]
                    , Feature.Editing.viewEditableText model "workspace" ws.id "name" ws.name
                    ]
                , div [ class "workspace-header-actions" ]
                    [ viewCreateButton model.activeTab
                    , if Permissions.canAdminCurrentWorkspace model then
                        button [ class "btn-small btn-danger-subtle", onClick (ConfirmWorkspaceDelete ws.id), disabled (model.groups.workspaceDeletion /= Nothing) ] [ text "Delete workspace" ]
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
                  else text "" ]
            , div [ class "workspace-details" ]
                [ div [ class "workspace-detail" ]
                    [ span [ class "workspace-detail-label" ] [ text "ID" ]
                    , Helpers.copyableValue "workspace-detail-value card-id card-id-copy" "workspace ID" ws.id ws.id
                    ]
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
        , if model.activeTab == AuditTab || model.activeTab == TimelineTab || model.activeTab == AdministrationTab || model.activeTab == SessionsTab then
            text ""
          else
            Feature.Search.viewSearchBar model
        , viewStickyWorkspaceBar model ws summaryParts
        , if not (Permissions.canReadCurrentWorkspace model) then
            viewUnauthorizedWorkspace model

          else
            div []
                [ viewTabs model
                , Feature.Search.viewNavigation model (if model.activeTab == AdministrationTab then
                    viewTabContent wsId model

                  else if model.dataLoading.loadingWorkspaceData then
                    div [ class "loading-indicator" ] [ text "Loading..." ]

                  else if model.activeTab == AuditTab || model.activeTab == TimelineTab || model.activeTab == SessionsTab then
                    viewTabContent wsId model

                  else if model.search.isSearching then
                    Feature.Search.viewUnifiedSearchLoading model

                  else
                    case model.search.searchError of
                        Just message ->
                            Feature.Search.viewUnifiedSearchError model message

                        Nothing ->
                            case model.search.unifiedResults of
                                Just results ->
                                    Feature.Search.viewUnifiedSearchResults model results

                                Nothing ->
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


viewStickyWorkspaceBar : Model -> Api.Workspace -> List String -> Html Msg
viewStickyWorkspaceBar model ws summaryParts =
    let
        stickyThreshold =
            200

        isVisible =
            model.mainContentScrollY > stickyThreshold
    in
    div
        [ class
            ("sticky-workspace-bar"
                ++ (if isVisible then
                        " sticky-workspace-bar--visible"

                    else
                        ""
                   )
            )
        ]
        [ div [ class "sticky-workspace-bar__header" ]
            [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                [ text (Api.workspaceTypeToString ws.workspaceType) ]
            , span [ class "sticky-workspace-name" ] [ text ws.name ]
            , span [ class "sticky-workspace-summary" ]
                [ viewPlanningSummary model summaryParts
                , if ws.workspaceType == Api.Repository then
                    let counts = model.observations.workspaceCounts in
                    span [ class "workspace-observation-total" ]
                        [ text ((if List.isEmpty summaryParts then "" else " · ") ++
                            (if counts.current then counts.value |> Maybe.map (\value -> String.fromInt value.totalCount ++ " observations") |> Maybe.withDefault "Observation total unavailable"
                             else if counts.error /= Nothing then "Observation total unavailable"
                             else if counts.value /= Nothing then "Updating Observation total…" else "Loading Observation total…"))
                        , if counts.error /= Nothing then button [ class "btn btn-sm", type_ "button", onClick RetryWorkspaceObservationCounts ] [ text "Retry counts" ] else text "" ]
                  else text "" ]
            ]
        , Feature.Search.viewWorkspaceSearchBar model
        ]


viewCreateButton : WorkspaceTab -> Html Msg
viewCreateButton _ =
    text ""


viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "tabs workspace-tabs", attribute "aria-label" "Workspace sections" ]
        (List.filterMap identity
            [ Just (viewTab ProjectsTab model.activeTab (workspaceTabLabel ProjectsTab))
            , Just (viewTab SessionsTab model.activeTab (workspaceTabLabel SessionsTab))
            , Just (viewTab ObservationsTab model.activeTab (workspaceTabLabel ObservationsTab))
            , Just (viewTab TimelineTab model.activeTab (workspaceTabLabel TimelineTab))
            , if Permissions.canViewCurrentWorkspaceAudit model then
                Just (viewTab AuditTab model.activeTab (workspaceTabLabel AuditTab))

              else
                Nothing
            , if Permissions.canViewWorkspaceAdministration model then
                Just (viewTab AdministrationTab model.activeTab (workspaceTabLabel AdministrationTab))

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

        AuditTab ->
            "Audit"

        AdministrationTab ->
            "Administration"


viewTab : WorkspaceTab -> WorkspaceTab -> String -> Html Msg
viewTab tab activeTab label =
    button
        [ class
            (if tab == activeTab then
                "tab active"

             else
                "tab"
            )
        , onClick (SwitchTab tab)
        , type_ "button"
        , attribute "aria-current" (if tab == activeTab then "page" else "false")
        ]
        [ text label ]


viewTabContent : String -> Model -> Html Msg
viewTabContent wsId model =
    case model.activeTab of
        ProjectsTab ->
            Feature.Cards.viewProjectsTree wsId model

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

        AuditTab ->
            Feature.AuditLog.viewWorkspaceAuditPanel wsId model

        AdministrationTab ->
            case Dict.get wsId model.workspaces of
                Just workspace ->
                    Feature.WorkspaceAdmin.viewWorkspaceAdminPanel workspace model

                Nothing ->
                    div [ class "loading-indicator" ] [ text "Loading workspace administration..." ]
