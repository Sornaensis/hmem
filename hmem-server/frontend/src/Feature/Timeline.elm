module Feature.Timeline exposing (ensureLoaded, init, sortTimelineEvents, timelineEventLabel, timelineEventToneClass, timelineStatusSummary, update, viewWorkspaceTimelinePanel)

import Api
import Browser.Navigation as Nav
import Char
import Helpers exposing (buildFragment, formatDate)
import Html exposing (..)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)
import String
import Types exposing (..)


init : TimelineModel
init =
    { events = []
    , hasMore = False
    , loading = False
    , error = Nothing
    , loadedWorkspaceId = Nothing
    }


ensureLoaded : String -> String -> TimelineModel -> ( TimelineModel, Cmd Msg )
ensureLoaded apiUrl wsId timeline =
    if timeline.loading || timeline.loadedWorkspaceId == Just wsId then
        ( timeline, Cmd.none )

    else
        ( { timeline
            | loading = True
            , error = Nothing
            , events = []
            , hasMore = False
          }
        , Api.fetchWorkspaceTimeline apiUrl wsId (GotWorkspaceTimeline wsId)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateToTimelineEntity entityType entityId ->
            case model.selectedWorkspaceId of
                Just wsId ->
                    let
                        targetTab =
                            ProjectsTab

                        focusEntry =
                            ( entityType, entityId )

                        focusModel =
                            model.focus

                        newHistory =
                            List.take (focusModel.historyIndex + 1) focusModel.history ++ [ focusEntry ]

                        newIndex =
                            List.length newHistory - 1

                        updatedFocus =
                            { focusModel
                                | focusedEntity = Just focusEntry
                                , breadcrumbAnchor = Just focusEntry
                                , history = newHistory
                                , historyIndex = newIndex
                            }

                        currentSearch =
                            model.search

                        updatedSearch =
                            { currentSearch | unifiedResults = Nothing, isSearching = False }
                    in
                    ( { model | activeTab = targetTab, focus = updatedFocus, search = updatedSearch }
                    , Nav.pushUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment targetTab (Just focusEntry))
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotWorkspaceTimeline wsId result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else
                let
                    currentTimeline =
                        model.timeline
                in
                case result of
                    Ok paginated ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | events = paginated.items
                                    , hasMore = paginated.hasMore
                                    , loading = False
                                    , error = Nothing
                                    , loadedWorkspaceId = Just wsId
                                }
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        ( { model
                            | timeline =
                                { currentTimeline
                                    | loading = False
                                    , error = Just "Failed to load timeline events."
                                    , loadedWorkspaceId = Nothing
                                }
                          }
                        , Cmd.none
                        )

        _ ->
            ( model, Cmd.none )


viewWorkspaceTimelinePanel : String -> Model -> Html Msg
viewWorkspaceTimelinePanel wsId model =
    let
        timeline =
            model.timeline
    in
    div [ class "timeline-panel" ]
        [ div [ class "timeline-panel-header" ]
            [ h3 [] [ text "Timeline" ]
            , p [] [ text "Curated task and project lifecycle events for this workspace." ]
            ]
        , if timeline.loading && List.isEmpty timeline.events then
            div [ class "loading-indicator" ] [ text "Loading timeline..." ]

          else
            viewTimelineBody wsId timeline
        ]


viewTimelineBody : String -> TimelineModel -> Html Msg
viewTimelineBody wsId timeline =
    case timeline.error of
        Just message ->
            div [ class "empty-state timeline-state" ]
                [ h3 [] [ text "Timeline unavailable" ]
                , p [] [ text message ]
                , button [ class "btn-secondary", onClick (SwitchTab TimelineTab) ] [ text "Retry" ]
                ]

        Nothing ->
            if List.isEmpty timeline.events then
                div [ class "empty-state timeline-state" ]
                    [ h3 [] [ text "No timeline events yet" ]
                    , p [] [ text "Create or complete tasks and projects to populate this workspace timeline." ]
                    ]

            else
                let
                    sortedEvents =
                        sortTimelineEvents timeline.events
                in
                div []
                    [ if timeline.hasMore then
                        p [ class "timeline-more-note" ] [ text "Showing the latest 50 timeline events." ]

                      else
                        text ""
                    , div [ class "timeline-event-list", title ("Timeline for workspace " ++ wsId) ]
                        (List.map viewTimelineEvent sortedEvents)
                    ]


sortTimelineEvents : List Api.WorkspaceTimelineEvent -> List Api.WorkspaceTimelineEvent
sortTimelineEvents events =
    List.sortWith compareTimelineEvents events


compareTimelineEvents : Api.WorkspaceTimelineEvent -> Api.WorkspaceTimelineEvent -> Order
compareTimelineEvents left right =
    case compare right.occurredAt left.occurredAt of
        EQ ->
            compare (timelineTieBreaker right) (timelineTieBreaker left)

        order ->
            order


timelineTieBreaker : Api.WorkspaceTimelineEvent -> String
timelineTieBreaker event =
    event.sourceAuditId |> Maybe.withDefault event.id


viewTimelineEvent : Api.WorkspaceTimelineEvent -> Html Msg
viewTimelineEvent event =
    button
        [ class ("timeline-event-card " ++ timelineEventToneClass event.eventType ++ " timeline-entity-" ++ event.entityType)
        , onClick (NavigateToTimelineEntity event.navigation.entityType event.navigation.entityId)
        , title ("Open " ++ String.toLower (entityTypeLabel event.entityType))
        ]
        [ div [ class "timeline-event-rail" ]
            [ span [ class "timeline-event-marker" ] [ text (timelineEventIcon event.eventType) ] ]
        , div [ class "timeline-event-content" ]
            [ div [ class "timeline-event-topline" ]
                [ span [ class "timeline-event-label" ] [ text (timelineEventLabel event.eventType) ]
                , span [ class ("timeline-entity-pill timeline-entity-pill-" ++ event.entityType) ] [ text (entityTypeLabel event.entityType) ]
                , span [ class "timeline-event-time" ] [ text (formatDate event.occurredAt) ]
                ]
            , div [ class "timeline-event-title-row" ]
                [ span [ class "timeline-event-title" ] [ text event.title ]
                , span [ class "timeline-event-action" ] [ text ("Open " ++ entityTypeLabel event.entityType) ]
                ]
            , viewStatusTransition event.statusTransition
            , viewTimelineContext event
            ]
        ]


viewStatusTransition : Maybe Api.TimelineStatusTransition -> Html Msg
viewStatusTransition maybeTransition =
    case maybeTransition of
        Just transition ->
            div [ class "timeline-status-transition" ]
                [ span [ class "timeline-status-label" ] [ text "Status" ]
                , span [ class "timeline-status-from" ] [ text (formatTimelineStatus transition.from) ]
                , span [ class "timeline-status-arrow" ] [ text "→" ]
                , span [ class "timeline-status-to" ] [ text (formatTimelineStatus transition.to) ]
                ]

        Nothing ->
            text ""


viewTimelineContext : Api.WorkspaceTimelineEvent -> Html Msg
viewTimelineContext event =
    let
        contextItems =
            timelineContextSegments event
    in
    if List.isEmpty contextItems then
        text ""

    else
        div [ class "timeline-event-context" ]
            (List.map viewTimelineContextChip contextItems)


viewTimelineContextChip : String -> Html Msg
viewTimelineContextChip label =
    span [ class "timeline-context-chip" ] [ text label ]


timelineContextSegments : Api.WorkspaceTimelineEvent -> List String
timelineContextSegments event =
    List.filterMap identity
        [ eventActorLabel event.actor
        , event.project |> Maybe.map (\project -> "Project: " ++ project.name)
        , event.parentTask |> Maybe.map (\task -> "Parent: " ++ task.title)
        ]


eventActorLabel : Maybe Api.TimelineActor -> Maybe String
eventActorLabel maybeActor =
    maybeActor
        |> Maybe.andThen
            (\actor ->
                case ( actor.actorLabel, actor.actorType ) of
                    ( Just label, _ ) ->
                        Just ("By " ++ label)

                    ( Nothing, Just actorType ) ->
                        Just ("By " ++ actorType)

                    _ ->
                        Nothing
            )


timelineStatusSummary : Maybe Api.TimelineStatusTransition -> Maybe String
timelineStatusSummary maybeTransition =
    maybeTransition
        |> Maybe.map (\transition -> formatTimelineStatus transition.from ++ " → " ++ formatTimelineStatus transition.to)


formatTimelineStatus : String -> String
formatTimelineStatus status =
    status
        |> String.replace "_" " "
        |> capitalizeFirst


capitalizeFirst : String -> String
capitalizeFirst value =
    case String.uncons value of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            ""


entityTypeLabel : String -> String
entityTypeLabel entityType =
    case entityType of
        "project" ->
            "Project"

        "subtask" ->
            "Subtask"

        "task" ->
            "Task"

        _ ->
            entityType


timelineEventLabel : String -> String
timelineEventLabel eventType =
    case eventType of
        "project_created" ->
            "Project created"

        "project_completed" ->
            "Project completed"

        "project_archived" ->
            "Project archived"

        "task_created" ->
            "Task created"

        "subtask_created" ->
            "Subtask created"

        "task_completed" ->
            "Task completed"

        "subtask_completed" ->
            "Subtask completed"

        "task_cancelled" ->
            "Task cancelled"

        "subtask_cancelled" ->
            "Subtask cancelled"

        _ ->
            eventType


timelineEventToneClass : String -> String
timelineEventToneClass eventType =
    if String.contains "created" eventType then
        "timeline-event-created"

    else if String.contains "archived" eventType then
        "timeline-event-archived"

    else if String.contains "cancelled" eventType then
        "timeline-event-cancelled"

    else if String.contains "completed" eventType then
        "timeline-event-completed"

    else
        "timeline-event-neutral"


timelineEventIcon : String -> String
timelineEventIcon eventType =
    if String.contains "created" eventType then
        "+"

    else if String.contains "archived" eventType then
        "A"

    else if String.contains "cancelled" eventType then
        "×"

    else
        "✓"
