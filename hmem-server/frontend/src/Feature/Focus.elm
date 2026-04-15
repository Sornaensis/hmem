module Feature.Focus exposing (buildProjectBreadcrumb, buildTaskBreadcrumb, update, viewFocusBreadcrumbBar, viewTaskBreadcrumb)

import Api
import Dict
import Helpers exposing (replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusEntity entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                -- Truncate any forward history, then append
                newHistory =
                    List.take (model.focusHistoryIndex + 1) model.focusHistory ++ [ entry ]

                newIndex =
                    List.length newHistory - 1

                newModel =
                    { model
                        | focusedEntity = Just entry
                        , breadcrumbAnchor = Just entry
                        , focusHistory = newHistory
                        , focusHistoryIndex = newIndex
                    }
            in
            ( newModel, replaceFragment newModel )

        FocusEntityKeepForward entityType entityId ->
            let
                entry =
                    ( entityType, entityId )

                -- Only change what's focused, don't modify history
                newModel =
                    { model | focusedEntity = Just entry }
            in
            ( newModel, replaceFragment newModel )

        FocusBreadcrumbNav idx ->
            let
                entry =
                    model.focusHistory
                        |> List.drop idx
                        |> List.head

                newModel =
                    { model
                        | focusedEntity = entry
                        , breadcrumbAnchor = entry
                        , focusHistoryIndex = idx
                    }
            in
            ( newModel, replaceFragment newModel )

        ClearFocus ->
            let
                newModel =
                    { model
                        | focusedEntity = Nothing
                        , breadcrumbAnchor = Nothing
                        , focusHistory = []
                        , focusHistoryIndex = 0
                    }
            in
            ( newModel, replaceFragment newModel )

        _ ->
            ( model, Cmd.none )


viewFocusBreadcrumbBar : Model -> Html Msg
viewFocusBreadcrumbBar model =
    case model.breadcrumbAnchor of
        Just ( aType, aId ) ->
            let
                -- Tree-based parent chain for the breadcrumb anchor (deepest focused entity)
                treeCrumbs =
                    case aType of
                        "project" ->
                            case Dict.get aId model.projects of
                                Just proj ->
                                    buildProjectBreadcrumb model proj []

                                Nothing ->
                                    []

                        "task" ->
                            case Dict.get aId model.tasks of
                                Just task ->
                                    buildTaskBreadcrumb model task []

                                Nothing ->
                                    []

                        _ ->
                            []

                currentFocusId =
                    model.focusedEntity |> Maybe.map Tuple.second |> Maybe.withDefault ""

                treeCrumbLinks =
                    treeCrumbs
                        |> List.map
                            (\( cId, cName, cType ) ->
                                if cId == currentFocusId then
                                    span [ class "focus-crumb focus-crumb-current" ] [ text cName ]

                                else
                                    span
                                        [ class "focus-crumb focus-crumb-link"
                                        , onClick (FocusEntityKeepForward cType cId)
                                        ]
                                        [ text cName ]
                            )
                        |> List.intersperse (span [ class "focus-crumb-sep" ] [ text " › " ])

                -- Forward history entries
                -- If the user navigated to a parent (focusedEntity differs from history entry),
                -- include the history entry at the current index as part of forward crumbs
                historyEntry =
                    model.focusHistory
                        |> List.drop model.focusHistoryIndex
                        |> List.head

                isOnParent =
                    historyEntry /= model.focusedEntity

                forwardStartIdx =
                    if isOnParent then
                        model.focusHistoryIndex

                    else
                        model.focusHistoryIndex + 1

                forwardCrumbs =
                    model.focusHistory
                        |> List.drop forwardStartIdx
                        |> List.indexedMap
                            (\i ( fType, fId ) ->
                                let
                                    name =
                                        case fType of
                                            "project" ->
                                                Dict.get fId model.projects |> Maybe.map .name |> Maybe.withDefault "Project"

                                            "task" ->
                                                Dict.get fId model.tasks |> Maybe.map .title |> Maybe.withDefault "Task"

                                            _ ->
                                                "Entity"

                                    -- Skip forward entries that are already in the tree crumbs
                                    isDuplicate =
                                        List.any (\( cId, _, _ ) -> cId == fId) treeCrumbs
                                in
                                if isDuplicate then
                                    Nothing

                                else
                                    Just
                                        ( span
                                            [ class "focus-crumb focus-crumb-forward"
                                            , onClick (FocusBreadcrumbNav (forwardStartIdx + i))
                                            ]
                                            [ text name ]
                                        )
                            )
                        |> List.filterMap identity
                        |> List.intersperse (span [ class "focus-crumb-sep" ] [ text " › " ])

                forwardSection =
                    if List.isEmpty forwardCrumbs then
                        []

                    else
                        span [ class "focus-crumb-sep" ] [ text " › " ] :: forwardCrumbs
            in
            div [ class "focus-breadcrumb-bar" ]
                (button [ class "focus-clear-btn", onClick ClearFocus, title "Exit focus mode" ] [ text "✕" ]
                    :: span [ class "focus-crumb focus-crumb-link", onClick ClearFocus ] [ text "All" ]
                    :: (if not (List.isEmpty treeCrumbLinks) then
                            span [ class "focus-crumb-sep" ] [ text " › " ] :: treeCrumbLinks ++ forwardSection

                        else
                            []
                       )
                )

        Nothing ->
            text ""


viewTaskBreadcrumb : Model -> Api.Task -> Html Msg
viewTaskBreadcrumb model task =
    let
        crumbs =
            buildTaskBreadcrumb model task []
    in
    if List.length crumbs <= 1 then
        text ""

    else
        div [ class "breadcrumb" ]
            (List.intersperse (span [ class "breadcrumb-sep" ] [ text "›" ])
                (List.map
                    (\( eid, label, etype ) ->
                        span
                            [ class ("breadcrumb-item breadcrumb-" ++ etype)
                            , onClick (FocusEntity etype eid)
                            , title ("Jump to " ++ label)
                            ]
                            [ text label ]
                    )
                    crumbs
                )
            )


buildTaskBreadcrumb : Model -> Api.Task -> List ( String, String, String ) -> List ( String, String, String )
buildTaskBreadcrumb model task acc =
    let
        currentCrumb =
            ( task.id, task.title, "task" )

        withParentTask =
            case task.parentId of
                Just pid ->
                    case Dict.get pid model.tasks of
                        Just parentTask ->
                            buildTaskBreadcrumb model parentTask (currentCrumb :: acc)

                        Nothing ->
                            currentCrumb :: acc

                Nothing ->
                    currentCrumb :: acc
    in
    case task.projectId of
        Just projId ->
            case Dict.get projId model.projects of
                Just proj ->
                    buildProjectBreadcrumb model proj withParentTask

                Nothing ->
                    withParentTask

        Nothing ->
            withParentTask


buildProjectBreadcrumb : Model -> Api.Project -> List ( String, String, String ) -> List ( String, String, String )
buildProjectBreadcrumb model project acc =
    let
        currentCrumb =
            ( project.id, project.name, "project" )
    in
    case project.parentId of
        Just pid ->
            case Dict.get pid model.projects of
                Just parentProj ->
                    buildProjectBreadcrumb model parentProj (currentCrumb :: acc)

                Nothing ->
                    currentCrumb :: acc

        Nothing ->
            currentCrumb :: acc
