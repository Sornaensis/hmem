module Page.Workspace exposing
    ( viewWorkspacePage
    )

import Api
import Dict
import Feature.Cards
import Feature.Editing
import Feature.Memory
import Feature.Search
import Helpers exposing (formatDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)


viewWorkspacePage : String -> Model -> Html Msg
viewWorkspacePage wsId model =
    case Dict.get wsId model.workspaces of
        Nothing ->
            div [ class "page" ]
                [ div [ class "loading-indicator" ] [ text "Loading..." ] ]

        Just ws ->
            let
                wsProjects =
                    model.projects |> Dict.values |> List.filter (\p -> p.workspaceId == wsId)

                wsTasks =
                    model.tasks |> Dict.values |> List.filter (\t -> t.workspaceId == wsId)

                wsMemories =
                    model.memories |> Dict.values |> List.filter (\m -> m.workspaceId == wsId)

                activeProjects =
                    wsProjects |> List.filter (\p -> p.status == Api.ProjActive || p.status == Api.ProjPaused) |> List.length

                activeTasks =
                    wsTasks |> List.filter (\t -> t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked) |> List.length

                memoryCount =
                    List.length wsMemories

                summaryParts =
                    List.filterMap identity
                        [ if activeProjects > 0 then
                            Just (String.fromInt activeProjects ++ " open " ++ (if activeProjects > 1 then "projects" else "project"))

                          else
                            Nothing
                        , if activeTasks > 0 then
                            Just (String.fromInt activeTasks ++ " open " ++ (if activeTasks > 1 then "tasks" else "task"))

                          else
                            Nothing
                        , if memoryCount > 0 then
                            Just (String.fromInt memoryCount ++ " memor" ++ (if memoryCount > 1 then "ies" else "y"))

                          else
                            Nothing
                        ]
            in
            div [ class "page" ]
                [ viewStickyWorkspaceBar model ws summaryParts
                , div [ class "workspace-header" ]
                    [ div [ class "workspace-header-top" ]
                        [ div [ class "workspace-header-title" ]
                            [ span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                                [ text (Api.workspaceTypeToString ws.workspaceType) ]
                            , Feature.Editing.viewEditableText model "workspace" ws.id "name" ws.name
                            ]
                        , viewCreateButton model.activeTab
                        ]
                    , if not (List.isEmpty summaryParts) then
                        div [ class "workspace-summary" ] [ text (String.join " · " summaryParts) ]

                      else
                        text ""
                    , div [ class "workspace-details" ]
                        [ div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "ID" ]
                            , span [ class "workspace-detail-value card-id card-id-copy", onClick (CopyId ws.id) ] [ text ws.id ]
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
                            , span [ class "workspace-detail-value" ] [ text (formatDate ws.createdAt) ]
                            ]
                        , div [ class "workspace-detail" ]
                            [ span [ class "workspace-detail-label" ] [ text "Updated" ]
                            , span [ class "workspace-detail-value" ] [ text (formatDate ws.updatedAt) ]
                            ]
                        ]
                    ]
                , Feature.Search.viewSearchBar model
                , viewTabs model.activeTab
                , if model.dataLoading.loadingWorkspaceData then
                    div [ class "loading-indicator" ] [ text "Loading..." ]

                  else
                    case model.search.unifiedResults of
                        Just results ->
                            Feature.Search.viewUnifiedSearchResults (Feature.Memory.viewMemoryCard model) model results

                        Nothing ->
                            viewTabContent wsId model
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
            , if not (List.isEmpty summaryParts) then
                span [ class "sticky-workspace-summary" ] [ text (String.join " · " summaryParts) ]

              else
                text ""
            ]
        ]


viewCreateButton : WorkspaceTab -> Html Msg
viewCreateButton _ =
    text ""


viewTabs : WorkspaceTab -> Html Msg
viewTabs activeTab =
    div [ class "tabs" ]
        [ viewTab ProjectsTab activeTab "Projects"
        , viewTab MemoriesTab activeTab "Memories"
        ]


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
        ]
        [ text label ]


viewTabContent : String -> Model -> Html Msg
viewTabContent wsId model =
    case model.activeTab of
        ProjectsTab ->
            Feature.Cards.viewProjectsTree wsId model

        MemoriesTab ->
            Feature.Memory.viewMemoriesList wsId model
