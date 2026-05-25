module Feature.Timeline exposing (ensureLoaded, init, timelineEventLabel, update, viewWorkspaceTimelinePanel)

import Api
import Browser.Navigation as Nav
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
                div []
                    [ if timeline.hasMore then
                        p [ class "timeline-more-note" ] [ text "Showing the latest 50 timeline events." ]

                      else
                        text ""
                    , div [ class "timeline-event-list", title ("Timeline for workspace " ++ wsId) ]
                        (List.map viewTimelineEvent timeline.events)
                    ]


viewTimelineEvent : Api.WorkspaceTimelineEvent -> Html Msg
viewTimelineEvent event =
    button
        [ class ("timeline-event-row timeline-event-" ++ event.entityType)
        , onClick (NavigateToTimelineEntity event.navigation.entityType event.navigation.entityId)
        , title ("Go to " ++ event.entityType)
        ]
        [ div [ class "timeline-event-marker" ] [ text (timelineEventIcon event.eventType) ]
        , div [ class "timeline-event-content" ]
            [ div [ class "timeline-event-main" ]
                [ span [ class "timeline-event-label" ] [ text (timelineEventLabel event.eventType) ]
                , span [ class "timeline-event-title" ] [ text event.title ]
                ]
            , div [ class "timeline-event-meta" ]
                (List.intersperse (span [ class "timeline-separator" ] [ text "·" ])
                    (List.map (span [] << List.singleton << text) (timelineMetaSegments event))
                )
            ]
        ]


timelineMetaSegments : Api.WorkspaceTimelineEvent -> List String
timelineMetaSegments event =
    List.filterMap identity
        [ Just (entityTypeLabel event.entityType)
        , Just (formatDate event.occurredAt)
        , event.statusTransition
            |> Maybe.map (\transition -> transition.from ++ " → " ++ transition.to)
        , eventActorLabel event.actor
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
