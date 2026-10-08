module CardsFocusNavigationTest exposing (suite)

import Api
import Main
import Html.Attributes
import Array
import HierarchyViewport as Viewport
import Json.Encode as Encode
import UpdateRouter
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.DataLoading as DataLoading
import Feature.Focus as Focus
import Helpers
import Http
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (Flags, Model, Msg(..), Page(..), WorkspaceTab(..), AuthStatus(..))
import Url


suite : Test
suite =
    describe "bounded Cards and Focus loading"
        [ test "project activity accents follow authoritative descendant rollups with no cached tasks" <| \_ ->
            let
                base = project "activity"
                rollup = base.readinessRollup
                active = { base | readinessRollup = { rollup | inProgressTaskCount = 2 } }
                seeded = DataLoading.mergeNavigationSummaries [ active ] [] model
                dependencies = seeded.dependencies
                cards = seeded.cards
                fallback = { seeded | tasks = Dict.empty, dependencies = { dependencies | projectReadinessRollups = Dict.empty }, cards = { cards | collapsedNodes = Dict.singleton "proj-activity" True } }
                stopped = DataLoading.mergeNavigationSummaries [ base ] [] fallback
            in
            Expect.all
                [ \_ -> Cards.viewProjectsTree workspaceId fallback |> Query.fromHtml |> Query.has [ Selector.class "card-project-in-progress" ]
                , \_ -> Cards.viewProjectsTree workspaceId stopped |> Query.fromHtml |> Query.hasNot [ Selector.class "card-project-in-progress" ]
                ] ()
        , test "offscreen partitioned task families keep canonical parent status and independent child status" <| \_ ->
            let
                parentBase = task "parent" Nothing
                parent = { parentBase | status = Api.InProgress, hasChildren = True, directSubtaskCount = 80 }
                children = List.range 1 80 |> List.map (\n -> let child = task ("child-" ++ String.padLeft 3 '0' (String.fromInt n)) (Just parent.id) in { child | status = Api.Done })
                ready = DataLoading.mergeNavigationSummaries [] (parent :: children) (rootSeed []) |> viewportReady
                target = "task:child-060"
                position = Dict.get target ready.cards.viewport.index.positions |> Maybe.withDefault 0
                editing = ready.editing
                pinned = { ready | editing = { editing | inlineCreate = Just (Types.InlineCreateTask { projectId = Nothing, parentId = Just "child-060", title = "" }) } }
                scrolled = Cards.updateViewport (viewportEvent pinned (Viewport.offset position ready.cards.viewport.index) [] Nothing []) pinned |> Tuple.first
                summaryOnly = { scrolled | tasks = Dict.remove parent.id scrolled.tasks }
                familyCheck source = Cards.viewProjectsTree workspaceId source |> Query.fromHtml
                    |> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-task-family" "parent") ]
                    |> Query.each (Query.has [ Selector.class "card-status-in_progress" ])
            in
            Expect.all
                [ \_ -> Expect.equal False (List.member "task:parent" (Cards.mountedViewportKeys scrolled))
                , \_ -> familyCheck scrolled
                , \_ -> familyCheck summaryOnly
                , \_ -> Cards.viewProjectsTree workspaceId scrolled |> Query.fromHtml |> Query.has [ Selector.class "card-subtask", Selector.class "card-status-done" ]
                ] ()
        , test "collapsed project counts retain authoritative totals when readiness caches are reset" <|
            \_ ->
                let
                    base =
                        project "counted"

                    rollup =
                        base.readinessRollup

                    summary =
                        { base | directTaskCount = 75, readinessRollup = { rollup | openTaskCount = 60, doneTaskCount = 10, cancelledTaskCount = 5 } }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ summary ] [] model

                    dependencies =
                        seeded.dependencies

                    cards =
                        seeded.cards

                    recovering =
                        { seeded | dependencies = { dependencies | projectReadinessRollups = Dict.empty }, cards = { cards | collapsedNodes = Dict.singleton "proj-counted" True }, tasks = Dict.empty }
                in
                Cards.viewProjectsTree workspaceId recovering
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "60/75 tasks remaining" ]
        , test "known empty project counts remain visible" <|
            \_ ->
                DataLoading.mergeNavigationSummaries [ project "empty" ] [] model
                    |> Cards.viewProjectsTree workspaceId
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "0 tasks" ]
        , test "unknown project counts render unavailable instead of silently disappearing" <|
            \_ ->
                let
                    base =
                        project "unknown"
                in
                { model | projects = Dict.singleton base.id (Api.projectFromCardSummary base) }
                    |> Cards.viewProjectsTree workspaceId
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Task counts unavailable" ]
        , test "an unloaded expanded project card starts exactly its branch request" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "parent" ] [] model

                    expanded =
                        Cards.update (ToggleCardExpand "parent") seeded
                            |> Tuple.first |> rootPaint
                in
                case Dict.get "project:parent" expanded.dataLoading.loadedNavigationBranches of
                    Just request ->
                        Expect.equal
                            { workspace = workspaceId, offset = 0, inFlight = True, succeeded = False }
                            { workspace = request.workspaceId, offset = request.projectOffset, inFlight = request.inFlight, succeeded = request.succeeded }

                    Nothing ->
                        Expect.fail "Expected an unloaded project expansion to request its branch"
        , test "Expand All hydrates and loads a visible project that was collapsed before its branch arrived" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "expand-all-parent" ] [] model

                    cards =
                        seeded.cards

                    collapsed =
                        { seeded | cards = { cards | collapsedNodes = Dict.singleton "proj-expand-all-parent" True } }

                    expanded =
                        Cards.update ExpandAllNodes collapsed |> Tuple.first |> rootPaint
                in
                Expect.equal
                    { collapsed = False, branchInFlight = True, detailInFlight = True }
                    { collapsed = Dict.get "proj-expand-all-parent" expanded.cards.collapsedNodes |> Maybe.withDefault False
                    , branchInFlight = Dict.get "project:expand-all-parent" expanded.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight |> Maybe.withDefault False
                    , detailInFlight = Dict.get "expand-all-parent" expanded.dataLoading.projectCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False
                    }
        , test "an unhydrated description cannot be edited and a failed detail exposes a working retry" <|
            \_ ->
                let
                    summary =
                        project "description-state"

                    seeded =
                        DataLoading.mergeNavigationSummaries [ summary ] [] model

                    ( requested, _ ) =
                        DataLoading.ensureNavigationPresentation "workspace_root" Nothing seeded |> (\( next, command ) -> ( rootPaint next, command ))

                    pendingView =
                        Cards.viewProjectsTree workspaceId requested |> Query.fromHtml

                    firstRequest =
                        Dict.get summary.id requested.dataLoading.projectCardDetailRequests

                    failed =
                        case firstRequest of
                            Just request ->
                                DataLoading.update (GotProjectCardDetail request summary.id (Err Http.Timeout)) requested |> Tuple.first

                            Nothing ->
                                requested

                    failedView =
                        Cards.viewProjectsTree workspaceId failed |> Query.fromHtml

                    retried =
                        DataLoading.update (RetryCardDetail "project" summary.id) failed |> Tuple.first
                in
                Expect.all
                    [ \_ -> pendingView |> Query.has [ Selector.text "Loading description…" ]
                    , \_ -> pendingView |> Query.findAll [ Selector.text "Click to add description..." ] |> Query.count (Expect.equal 0)
                    , \_ -> failedView |> Query.has [ Selector.text "Description unavailable.", Selector.text "Retry" ]
                    , \_ ->
                        case ( firstRequest, Dict.get summary.id retried.dataLoading.projectCardDetailRequests ) of
                            ( Just first, Just second ) ->
                                Expect.equal True (second.inFlight && second.requestId > first.requestId)

                            _ ->
                                Expect.fail "Expected the failed detail request to restart"
                    ]
                    ()
        , test "direct focus requests only a target absent from the bounded card cache" <|
            \_ ->
                let
                    missing =
                        Focus.update (FocusEntity "project" "outside-root-page") model
                            |> Tuple.first

                    knownModel =
                        DataLoading.mergeNavigationSummaries [ project "inside-root-page" ] [] model

                    known =
                        Focus.update (FocusEntity "project" "inside-root-page") knownModel
                            |> Tuple.first
                in
                Expect.equal
                    { missingTarget = Just "outside-root-page"
                    , missingInFlight = True
                    , knownHasNoRequest = True
                    }
                    { missingTarget = Dict.get "project:outside-root-page" missing.dataLoading.navigationFocuses |> Maybe.map .entityId
                    , missingInFlight = missing.dataLoading.activeNavigationFocus |> Maybe.map .inFlight |> Maybe.withDefault False
                    , knownHasNoRequest = Dict.member "project:inside-root-page" known.dataLoading.navigationFocuses |> not
                    }
        , test "initial end-status placeholder cannot move scroll origin when real expanded roots arrive before paint" <| \_ ->
            let
                waiting = rootSeed [] |> viewportReady
                root = project "root"
                children = List.range 1 50 |> List.map (\n -> let child = project ("child-" ++ String.fromInt n) in { child | parentId = Just "root" })
                first = DataLoading.mergeNavigationSummaries [ root ] [] waiting
                firstPaint = Cards.refreshViewport waiting ( first, Cmd.none ) |> Tuple.first
                continued = DataLoading.mergeNavigationSummaries children [] firstPaint
                beforePaint = Cards.refreshViewport firstPaint ( continued, Cmd.none ) |> Tuple.first
            in
            Expect.equal ( 0, 0, True )
                ( firstPaint.cards.viewport.top, beforePaint.cards.viewport.top, List.member "project:root" (Cards.mountedViewportKeys beforePaint) )
        , test "filter lifetime preserves the offset through pending membership and measured replacement" <| \_ ->
            let
                seeded = rootSeed (List.range 1 50 |> List.map (\n -> project ("root-" ++ String.fromInt n))) |> viewportReady
                scrolled = Cards.updateViewport (viewportEvent seeded 500 [] Nothing []) seeded |> Tuple.first
                pending = routed (ToggleFilterProjectStatus "active") scrolled
                loaded = case pending.dataLoading.rootNavigationRequest of
                    Just request -> routed (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint 0 0
                        (Ok { workspaceId = workspaceId, projects = { items = [ project "replacement" ], hasMore = False }, tasks = { items = [], hasMore = False } })) pending
                    Nothing -> pending
                measured = Cards.updateViewport (viewportEvent loaded 150 [ ( "project:replacement", 400 ) ] Nothing []) loaded |> Tuple.first
                disclosed = routed CollapseAllNodes measured
            in
            Expect.all
                [ \_ -> Expect.equal ( 500, True, scrolled.cards.viewport.index.keys ) ( pending.cards.viewport.top, pending.cards.viewport.preserveScroll, pending.cards.viewport.index.keys )
                , \_ -> Expect.equal ( 500, True ) ( loaded.cards.viewport.top, loaded.cards.viewport.preserveScroll )
                , \_ -> Expect.equal 150 measured.cards.viewport.top
                , \_ -> Expect.equal False disclosed.cards.viewport.preserveScroll
                ] ()
        , test "settled optional pages and failures release filter extent, and changed contexts reset it" <| \_ ->
            let
                ready = rootSeed [ project "first" ] |> viewportReady
                pending = routed (ToggleFilterProjectStatus "active") ready
                loading = pending.dataLoading
                optional = { pending | dataLoading = { loading | navigationQueue = [], loadedNavigationBranches = Dict.empty, rootNavigationRequest = Maybe.map (\request -> { request | inFlight = False, succeeded = True, projectHasMore = True }) loading.rootNavigationRequest } }
                optionalLoading = optional.dataLoading
                failed = { optional | dataLoading = { optionalLoading | rootNavigationRequest = Maybe.map (\request -> { request | succeeded = False, projectHasMore = False }) optionalLoading.rootNavigationRequest } }
                otherSearch = optional.search
                pendingCards = pending.cards
                pendingViewport = pendingCards.viewport
                deep = { pending | cards = { pendingCards | viewport = { pendingViewport | top = 10000 } } }
                other = Cards.refreshViewport optional ( { optional | selectedWorkspaceId = Just "other-workspace", sessionRequestEpoch = optional.sessionRequestEpoch + 1, search = { otherSearch | filterProjectStatuses = [] } }, Cmd.none ) |> Tuple.first
                released source = Cards.viewProjectsTree workspaceId source |> Query.fromHtml |> Query.find [ Selector.id "hierarchy-viewport" ] |> Query.has [ Selector.attribute (Html.Attributes.style "min-height" "0px") ]
            in
            Expect.all
                [ \_ -> released optional
                , \_ -> released failed
                , \_ -> released deep
                , \_ -> Expect.equal ( False, 0 ) ( other.cards.viewport.preserveScroll, other.cards.viewport.filterExtent )
                ] ()
        , test "cached root rows remain scroll reachable with a bounded first paint" <| \_ ->
            let
                seeded = rootSeed (List.range 1 50 |> List.map (\number -> project ("root-" ++ String.padLeft 3 '0' (String.fromInt number))))
                ready = viewportReady seeded
                firstKeys = Cards.mountedViewportKeys ready
                lastPosition = Dict.get "project:root-050" ready.cards.viewport.index.positions |> Maybe.withDefault 0
                scrolled = Cards.updateViewport (viewportEvent ready (Viewport.offset lastPosition ready.cards.viewport.index) [] Nothing []) ready |> Tuple.first
                earlier = Cards.updateViewport (viewportEvent scrolled 0 [] Nothing []) scrolled |> Tuple.first
            in
            Expect.all
                [ \_ -> Expect.equal True (List.length firstKeys <= 25 && List.member "project:root-001" firstKeys)
                , \_ -> Expect.equal True (List.member "project:root-050" (Cards.mountedViewportKeys scrolled))
                , \_ -> Expect.equal True (List.member "project:root-001" (Cards.mountedViewportKeys earlier) && List.length (Cards.mountedViewportKeys earlier) <= 25)
                , \_ -> Expect.equal 50 (Dict.size scrolled.projects)
                , \_ -> Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml |> Query.findAll [ Selector.class "hierarchy-row" ] |> Query.count (\count -> Expect.equal True (count <= 25))
                , \_ -> Cards.viewProjectsTree workspaceId scrolled |> Query.fromHtml |> Query.find [ Selector.class "navigation-load-more" ] |> Query.has [ Selector.text "Load more projects" ]
                ] ()
        , test "root More requests the next transport page and appends without a presentation swap" <| \_ ->
            let
                seeded = rootSeed (List.range 1 50 |> List.map (\number -> project (String.fromInt number))) |> viewportReady
                requested = routed (LoadRootNavigationPage "project") seeded
                result = case requested.dataLoading.rootNavigationRequest of
                    Just request -> routed (GotRootNavigation workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint request.projectOffset request.taskOffset
                        (Ok { workspaceId = workspaceId, projects = { items = [ project "51", project "52" ], hasMore = False }, tasks = { items = [], hasMore = False } })) requested
                    Nothing -> requested
            in
            Expect.equal { offset = Just 50, inFlight = True, count = 52, earlier = True, later = True }
                { offset = requested.dataLoading.rootNavigationRequest |> Maybe.map .projectOffset
                , inFlight = requested.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False
                , count = Dict.size result.projects
                , earlier = Dict.member "project:1" result.cards.viewport.rows
                , later = Dict.member "project:52" result.cards.viewport.rows
                }
        , test "a direct offscreen scroll mounts the actual target before acknowledged scrolling" <| \_ ->
            let
                ready = rootSeed (List.range 1 100 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))) |> viewportReady
                mounted = Cards.updateViewport (viewportEvent ready 0 [] (Just "entity:p100") []) ready |> Tuple.first
            in
            Expect.equal ( Just "project:p100", True, True )
                ( mounted.cards.viewport.target, List.member "project:p100" (Cards.mountedViewportKeys mounted), List.length (Cards.mountedViewportKeys mounted) <= 26 )
        , test "native focus stays pinned independently of focus mode and stale context cannot replace it" <| \_ ->
            let
                ready = rootSeed (List.range 1 80 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))) |> viewportReady
                pinned = Cards.updateViewport (viewportEvent ready 20000 [] Nothing [ "project:p001" ]) ready |> Tuple.first
                stalePayload = Encode.object [ ( "workspace", Encode.string workspaceId ), ( "epoch", Encode.int pinned.sessionRequestEpoch ), ( "generation", Encode.int pinned.dataLoading.navigationGeneration ), ( "revision", Encode.int (pinned.cards.viewport.revision - 1) ), ( "top", Encode.float 0 ) ]
                stale = Cards.updateViewport stalePayload pinned |> Tuple.first
            in
            Expect.equal { pinned = True, focus = Nothing, top = pinned.cards.viewport.top, revision = pinned.cards.viewport.revision }
                { pinned = List.member "project:p001" (Cards.mountedViewportKeys stale), focus = stale.focus.focusedEntity, top = stale.cards.viewport.top, revision = stale.cards.viewport.revision }
        , test "measuring an earlier row preserves the current key and intrarow anchor without rebuilding rows" <| \_ ->
            let
                ready = rootSeed (List.range 1 80 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))) |> viewportReady
                position = Dict.get "project:p040" ready.cards.viewport.index.positions |> Maybe.withDefault 0
                top = Viewport.offset position ready.cards.viewport.index + 15
                scrolled = Cards.updateViewport (viewportEvent ready top [] Nothing []) ready |> Tuple.first
                measured = Cards.updateViewport (viewportEvent scrolled top [ ( "project:p001", 300 ) ] Nothing []) scrolled |> Tuple.first
            in
            Expect.equal ( 15, scrolled.cards.viewport.revision, scrolled.cards.viewport.rows )
                ( measured.cards.viewport.top - Viewport.offset position measured.cards.viewport.index, measured.cards.viewport.revision, measured.cards.viewport.rows )
        , test "logical drop boundaries retain priorities from siblings outside the mounted viewport" <| \_ ->
            let
                ready = rootSeed (List.range 1 80 |> List.map (\n -> let base = project ("p" ++ String.padLeft 3 '0' (String.fromInt n)) in { base | priority = 81 - n })) |> viewportReady
                dragging = { ready | dragDrop = { dragging = Just { entityType = "project", entityId = "p001" }, dragOver = Nothing, dropActionModal = Nothing } }
                updated = Cards.refreshViewport ready ( dragging, Cmd.none ) |> Tuple.first
                zone = Dict.get "drop:project:p040" updated.cards.viewport.rows |> Maybe.andThen .zone
            in
            Expect.equal ( Just ( Just 42, Just 41 ), True )
                ( zone |> Maybe.map (\value -> ( value.abovePriority, value.belowPriority )), List.length (Cards.mountedViewportKeys updated) <= 26 )
        , test "a pending offscreen target gets the next released detail slot before ordinary cards" <| \_ ->
            let
                ready = rootSeed (List.range 1 80 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))) |> viewportReady
                targeted = Cards.updateViewport (viewportEvent ready 0 [] (Just "entity:p080") []) ready |> Tuple.first
                completion = ready.dataLoading.projectCardDetailRequests |> Dict.toList |> List.head
                released = case completion of
                    Just ( id, request ) -> DataLoading.update (GotProjectCardDetail request id (Err Http.Timeout)) targeted |> Tuple.first
                    Nothing -> targeted
            in
            Expect.equal ( True, True )
                ( Dict.get "p080" released.dataLoading.projectCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False, Set.size released.dataLoading.cardDetailAdmissions <= 6 )
        , test "active editing and drag targets survive scrolling without mounting ancestor paths" <| \_ ->
            let
                ready = rootSeed (List.range 1 80 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))) |> viewportReady
                editing = ready.editing
                pinned = { ready | editing = { editing | inlineCreate = Just (Types.InlineCreateProject { parentId = Just "p001", name = "" }) }, dragDrop = { dragging = Just { entityType = "project", entityId = "p002" }, dragOver = Nothing, dropActionModal = Nothing } }
                scrolled = Cards.updateViewport (viewportEvent pinned 12000 [] Nothing []) pinned |> Tuple.first
            in
            Expect.equal ( True, True, True )
                ( List.member "project:p001" (Cards.mountedViewportKeys scrolled), List.member "project:p002" (Cards.mountedViewportKeys scrolled), List.length (Cards.mountedViewportKeys scrolled) <= 27 )
        , test "deep focus mounts and demands the target without its entire ancestor chain" <| \_ ->
            let
                summaries = List.range 1 100 |> List.map (\n -> let base = project ("deep-" ++ String.padLeft 3 '0' (String.fromInt n)) in { base | parentId = if n == 1 then Nothing else Just ("deep-" ++ String.padLeft 3 '0' (String.fromInt (n - 1))) })
                seeded = rootSeed summaries
                focused = routed (FocusEntity "project" "deep-100") { seeded | auth = { status = AuthReady, mode = Just "test" } }
            in
            Expect.equal ( True, True, True )
                ( List.member "project:deep-100" (Cards.mountedViewportKeys focused)
                , List.length (Cards.mountedViewportKeys focused) <= 25
                , Dict.keys focused.dataLoading.projectCardDetailRequests |> List.all ((==) "deep-100")
                )
        , test "reorder retains the scroll anchor while delete and reparent retire old row membership" <| \_ ->
            let
                summaries = List.range 1 80 |> List.map (\n -> project ("p" ++ String.padLeft 3 '0' (String.fromInt n)))
                ready = rootSeed summaries |> viewportReady
                position = Dict.get "project:p040" ready.cards.viewport.index.positions |> Maybe.withDefault 0
                top = Viewport.offset position ready.cards.viewport.index + 15
                scrolled = Cards.updateViewport (viewportEvent ready top [] Nothing []) ready |> Tuple.first
                promoted = let base = project "p040" in { base | priority = 10 }
                reorderedSource = DataLoading.mergeNavigationSummaries [ promoted ] [] scrolled
                reordered = Cards.refreshViewport scrolled ( reorderedSource, Cmd.none ) |> Tuple.first
                newPosition = Dict.get "project:p040" reordered.cards.viewport.index.positions |> Maybe.withDefault 0
                child = let base = project "p050" in { base | parentId = Just "p002" }
                movedSource = DataLoading.mergeNavigationSummaries [ child ] [] reordered
                moved = Cards.refreshViewport reordered ( movedSource, Cmd.none ) |> Tuple.first
                loading = moved.dataLoading
                deletedSource = { moved | projects = Dict.remove "p040" moved.projects, dataLoading = { loading | projectCardSummaries = Dict.remove "p040" loading.projectCardSummaries, navigationVisibleProjectIds = Set.remove "p040" loading.navigationVisibleProjectIds } }
                deleted = Cards.refreshViewport moved ( deletedSource, Cmd.none ) |> Tuple.first
            in
            Expect.equal ( 15, Just (Just "p002"), False )
                ( reordered.cards.viewport.top - Viewport.offset newPosition reordered.cards.viewport.index
                , Dict.get "project:p050" moved.cards.viewport.rows |> Maybe.map .parentId
                , Dict.member "project:p040" deleted.cards.viewport.rows
                )
        , test "content-only task detail response preserves cached viewport projection and geometry" <| \_ ->
            let
                seeded = DataLoading.mergeNavigationSummaries [] [ task "detail" Nothing ] model
                loading = seeded.dataLoading
                ready = rootPaint { seeded | dataLoading = { loading | navigationVisibilityActive = True, navigationVisibleTaskIds = Set.singleton "detail" } }
                taskDetail = Api.taskFromCardSummary (task "detail" Nothing)
            in
            case Dict.get "detail" ready.dataLoading.taskCardDetailRequests of
                Nothing -> Expect.fail "Expected a real admitted task detail request"
                Just request ->
                    let
                        updated = routed (GotTaskCardDetail request "detail" (Ok { taskDetail | description = Just "New full description" })) ready
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just (Just "New full description")) (Dict.get "detail" updated.tasks |> Maybe.map .description)
                        , \_ -> Expect.equal ready.cards.viewport.projection updated.cards.viewport.projection
                        , \_ -> Expect.equal ready.cards.viewport.index updated.cards.viewport.index
                        , \_ -> Expect.equal ready.cards.viewport.revision updated.cards.viewport.revision
                        ] ()
        , test "canonical prerequisite status changes refresh Done eligibility with unchanged links and geometry" <| \_ ->
            let
                seeded = DataLoading.mergeNavigationSummaries [] [ task "dependent" Nothing, task "prerequisite" Nothing ] model
                websocket = seeded.webSocket
                dependencies = seeded.dependencies
                loading = seeded.dataLoading
                ready = viewportReady { seeded | dataLoading = { loading | navigationVisibilityActive = True, navigationVisibleTaskIds = Set.fromList [ "dependent", "prerequisite" ] }, sessionContext = Just editorSession, dependencies = { dependencies | taskDependencyLinks = [ { taskId = "dependent", dependsOnId = "prerequisite" } ] }, webSocket = { websocket | targetGenerations = Dict.singleton "workspace:workspace-1|task:prerequisite" 1 } }
                guard = { scopeKey = "workspace:workspace-1", targetKey = "task:prerequisite", targetGeneration = 1, sessionEpoch = ready.sessionRequestEpoch, routeWorkspace = Just workspaceId, audienceId = "editor" }
                prerequisite = Api.taskFromCardSummary (task "prerequisite" Nothing)
                completed = routed (CanonicalTaskFetched guard "prerequisite" (Ok { prerequisite | status = Api.Done })) ready
                reopened = routed (CanonicalTaskFetched guard "prerequisite" (Ok prerequisite)) completed
                removed = routed (CanonicalTaskFetched guard "prerequisite" (Err (Http.BadStatus 404))) reopened
                restored = routed (CanonicalTaskFetched guard "prerequisite" (Ok prerequisite)) removed
                done source = Cards.viewProjectsTree workspaceId source |> Query.fromHtml |> Query.find [ Selector.id "entity-dependent" ] |> Query.findAll [ Selector.tag "option", Selector.attribute (Html.Attributes.value "done") ]
            in
            Expect.all
                [ \_ -> done ready |> Query.count (Expect.equal 0)
                , \_ -> done completed |> Query.count (Expect.equal 1)
                , \_ -> done reopened |> Query.count (Expect.equal 0)
                , \_ -> done removed |> Query.count (Expect.equal 1)
                , \_ -> done restored |> Query.count (Expect.equal 0)
                , \_ -> Expect.equal ready.cards.viewport.revision restored.cards.viewport.revision
                , \_ -> Expect.equal ready.cards.viewport.index reopened.cards.viewport.index
                ] ()
        , test "canonical overview link changes refresh rendered Done eligibility without rebuilding viewport geometry" <| \_ ->
            let
                seeded = DataLoading.mergeNavigationSummaries [] [ task "dependent" Nothing, task "prerequisite" Nothing ] model
                websocket = seeded.webSocket
                ready = viewportReady { seeded | sessionContext = Just editorSession, webSocket = { websocket | targetGenerations = Dict.singleton "workspace:workspace-1|task-overview:dependent" 1 } }
                guard = { scopeKey = "workspace:workspace-1", targetKey = "task-overview:dependent", targetGeneration = 1, sessionEpoch = ready.sessionRequestEpoch, routeWorkspace = Just workspaceId, audienceId = "editor" }
                overview dependencies = { task = Dict.get "dependent" ready.tasks |> Maybe.withDefault (Api.taskFromCardSummary (task "dependent" Nothing)), dependencies = dependencies, readinessRollup = (task "dependent" Nothing).readinessRollup }
                linked = routed (CanonicalTaskOverviewFetched guard "dependent" (Ok (overview [ { id = "prerequisite", name = "prerequisite" } ]))) ready
                unlinked = routed (CanonicalTaskOverviewFetched guard "dependent" (Ok (overview []))) linked
                done source = Cards.viewProjectsTree workspaceId source |> Query.fromHtml |> Query.find [ Selector.id "entity-dependent" ] |> Query.findAll [ Selector.tag "option", Selector.attribute (Html.Attributes.value "done") ]
            in
            Expect.all
                [ \_ -> done ready |> Query.count (Expect.equal 1)
                , \_ -> done linked |> Query.count (Expect.equal 0)
                , \_ -> done unlinked |> Query.count (Expect.equal 1)
                , \_ -> Expect.equal ready.cards.viewport.revision unlinked.cards.viewport.revision
                , \_ -> Expect.equal ready.cards.viewport.index unlinked.cards.viewport.index
                ] ()
        , test "Main URL changes rebuild cached focused rows and restore roots when browser focus clears" <| \_ ->
            let
                seeded = viewportReady (DataLoading.mergeNavigationSummaries [ project "a", project "b" ] [] model)
                focused = Main.update (UrlChanged { url | fragment = Just "tab=projects&focus=project:a" }) seeded |> Tuple.first
                cleared = Main.update (UrlChanged { url | fragment = Just "tab=projects" }) focused |> Tuple.first
                ids source = source.cards.viewport.rows |> Dict.values |> List.filter (\row -> row.kind == "project") |> List.map .entityId |> List.sort
            in
            Expect.equal { focused = [ "a" ], cleared = [ "a", "b" ], revisionChanged = True }
                { focused = ids focused, cleared = ids cleared, revisionChanged = focused.cards.viewport.revision > seeded.cards.viewport.revision }
        , test "each project owns a distinct terminal task drop boundary even for empty task lists" <| \_ ->
            let
                check populated =
                    let
                        taskA = let base = task "task-a" Nothing in { base | projectId = Just "a", priority = 8 }
                        taskB = let base = task "task-b" Nothing in { base | projectId = Just "b", priority = 3 }
                        seeded = DataLoading.mergeNavigationSummaries [ project "a", project "b" ] (if populated then [ taskA, taskB ] else []) model
                        dragging = { seeded | auth = { status = AuthReady, mode = Just "test" }, dragDrop = { dragging = Just { entityType = "task", entityId = "task-a" }, dragOver = Nothing, dropActionModal = Nothing } }
                        cached = Cards.refreshViewport seeded ( dragging, Cmd.none ) |> Tuple.first
                    in
                    cached.cards.viewport.rows |> Dict.values
                        |> List.filterMap (\row -> row.zone |> Maybe.andThen (\zone -> if zone.parentType == "project-tasks" && zone.belowPriority == Nothing then Just ( row.key, zone.projectId, zone.abovePriority ) else Nothing))
            in
            Expect.equal
                { empty = [ ( "drop:project-tasks:a:root:end", Just "a", Nothing ), ( "drop:project-tasks:b:root:end", Just "b", Nothing ) ]
                , populated = [ ( "drop:project-tasks:a:root:end", Just "a", Just 8 ), ( "drop:project-tasks:b:root:end", Just "b", Just 3 ) ] }
                { empty = check False, populated = check True }
        , test "collapsing a loaded branch unmounts its child cards without discarding branch membership" <|
            \_ ->
                let
                    parent =
                        project "parent"

                    child =
                        let
                            base =
                                project "child"
                        in
                        { base | parentId = Just "parent", hasChildren = False }

                    seeded =
                        DataLoading.mergeNavigationSummaries [ parent, child ] [] model

                    collapsed =
                        Cards.update (ToggleTreeNode "proj-parent") seeded |> Tuple.first

                    rendered =
                        Cards.viewProjectsTree workspaceId collapsed |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-project" ] rendered |> Query.count (Expect.equal 1)
                    , \_ -> Expect.equal True (Dict.member "child" collapsed.projects)
                    ]
                    ()
        , test "collapsed card extras stay unmounted while expanded cards retain editable project controls" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "editable-parent" ] [] model

                    collapsed =
                        Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml

                    expandedModel =
                        Cards.update (ToggleCardExpand "editable-parent") seeded |> Tuple.first

                    expanded =
                        Cards.viewProjectsTree workspaceId expandedModel |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-extras" ] collapsed |> Query.count (Expect.equal 0)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] expanded |> Query.count (Expect.equal 1)
                    , \_ -> Query.find [ Selector.class "card-project" ] expanded |> Query.has [ Selector.class "editable-text", Selector.text "editable-parent" ]
                    ]
                    ()
        , test "task details unmount while collapsed and remain editable when expanded" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [] [ task "editable-task" Nothing ] model

                    collapsed =
                        Cards.viewProjectsTree workspaceId seeded |> Query.fromHtml

                    expandedModel =
                        Cards.update (ToggleCardExpand "editable-task") seeded |> Tuple.first

                    expanded =
                        Cards.viewProjectsTree workspaceId expandedModel |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-task" ] collapsed |> Query.count (Expect.equal 1)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] collapsed |> Query.count (Expect.equal 0)
                    , \_ -> Query.findAll [ Selector.class "card-extras" ] expanded |> Query.count (Expect.equal 1)
                    , \_ -> Query.find [ Selector.class "card-task" ] expanded |> Query.has [ Selector.class "editable-text", Selector.text "editable-task" ]
                    ]
                    ()
        , test "focus and collapse reset reversible presentation cursors without dropping cached cards" <|
            \_ ->
                let
                    prepared =
                        DataLoading.prepareRootNavigationRequest (Just workspaceId) model

                    fingerprint =
                        prepared.dataLoading.rootNavigationRequest
                            |> Maybe.map .filterFingerprint
                            |> Maybe.withDefault ""

                    ( loaded, _ ) =
                        DataLoading.update
                            (GotRootNavigation workspaceId prepared.sessionRequestEpoch Nothing prepared.dataLoading.navigationGeneration fingerprint 0 0
                                (Ok { workspaceId = workspaceId, projects = { items = [ project "root" ], hasMore = True }, tasks = { items = [], hasMore = False } })
                            )
                            prepared

                    advanced =
                        DataLoading.update (LoadRootNavigationPage "project") loaded |> Tuple.first

                    focused =
                        Focus.update (FocusEntity "project" "root") advanced |> Tuple.first

                    collapsed =
                        Cards.update CollapseAllNodes advanced |> Tuple.first

                    projectOffset current =
                        current.dataLoading.rootNavigationPresentation |> Maybe.map .projectOffset
                in
                Expect.equal
                    { advanced = Just 1, focusReset = Just 0, collapseReset = Just 0, cached = True }
                    { advanced = projectOffset advanced
                    , focusReset = projectOffset focused
                    , collapseReset = projectOffset collapsed
                    , cached = Dict.member "root" collapsed.projects
                    }
        , test "the projection reads only visible membership while a large off-cache remains available for direct focus" <|
            \_ ->
                let
                    summaries =
                        List.range 1 100
                            |> List.map (\number -> project ("cached-" ++ String.fromInt number))

                    seeded =
                        DataLoading.mergeNavigationSummaries summaries [] model

                    loading =
                        seeded.dataLoading

                    visible =
                        { seeded
                            | dataLoading =
                                { loading
                                    | navigationVisibilityActive = True
                                    , navigationVisibleProjectIds = Set.singleton "cached-1"
                                }
                        }

                    projection =
                        Cards.cardTreeProjection workspaceId visible

                    tree =
                        Cards.viewProjectsTree workspaceId visible |> Query.fromHtml

                    focused =
                        Focus.update (FocusEntity "project" "cached-100") visible |> Tuple.first

                    focusedTree =
                        Cards.viewProjectsTree workspaceId focused |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> Query.findAll [ Selector.class "card-project" ] tree |> Query.count (Expect.equal 1)
                    , \_ -> Query.findAll [ Selector.class "card-project" ] focusedTree |> Query.count (Expect.equal 1)
                    , \_ -> Expect.equal 100 (Dict.size focused.projects)
                    , \_ -> Expect.equal [ "cached-1" ] (List.map .id projection.projects)
                    , \_ -> Expect.equal [] (List.map .id projection.tasks)
                    ]
                    ()
        , test "the pure projection keeps nested roots, children, project tasks, and task children ordered" <|
            \_ ->
                let
                    rootA =
                        project "root-a"

                    rootB =
                        project "root-b"

                    childA =
                        let
                            base =
                                project "child-a"
                        in
                        { base | parentId = Just "root-a" }

                    childB =
                        let
                            base =
                                project "child-b"
                        in
                        { base | parentId = Just "root-a" }

                    projectTaskA =
                        let
                            base =
                                task "project-task-a" Nothing
                        in
                        { base | projectId = Just "root-a" }

                    projectTaskB =
                        let
                            base =
                                task "project-task-b" Nothing
                        in
                        { base | projectId = Just "root-a" }

                    taskChildA =
                        let
                            base =
                                task "task-child-a" (Just "project-task-a")
                        in
                        { base | projectId = Just "root-a" }

                    taskChildB =
                        let
                            base =
                                task "task-child-b" (Just "project-task-a")
                        in
                        { base | projectId = Just "root-a" }

                    projection =
                        DataLoading.mergeNavigationSummaries
                            [ rootB, childB, rootA, childA ]
                            [ taskChildB, projectTaskB, taskChildA, projectTaskA ]
                            model
                            |> Cards.cardTreeProjection workspaceId

                    ids =
                        List.map .id
                in
                Expect.equal
                    { roots = [ "root-a", "root-b" ]
                    , children = [ "child-a", "child-b" ]
                    , projectTasks = [ "project-task-a", "project-task-b" ]
                    , taskChildren = [ "task-child-a", "task-child-b" ]
                    }
                    { roots = projection.projects |> List.filter (\item -> item.parentId == Nothing) |> ids
                    , children = Dict.get "root-a" projection.projectChildren |> Maybe.withDefault [] |> ids
                    , projectTasks = Dict.get "root-a" projection.projectTasks |> Maybe.withDefault [] |> ids
                    , taskChildren = Dict.get "project-task-a" projection.taskChildren |> Maybe.withDefault [] |> ids
                    }
        , test "presentation windows are exact across 25/50/51/75 boundaries and pin an out-of-window focus" <|
            \_ ->
                let
                    window total offset pins =
                        Cards.presentationWindow identity pins offset (List.range 1 total |> List.map String.fromInt)

                    first25 =
                        window 25 0 Set.empty

                    final50 =
                        window 50 25 Set.empty

                    final51 =
                        window 51 50 Set.empty

                    middle75 =
                        window 75 25 Set.empty

                    final80 =
                        window 80 75 Set.empty

                    pinned75 =
                        window 75 0 (Set.singleton "75")

                    fullyPinned =
                        window 75 25 (List.range 51 75 |> List.map String.fromInt |> Set.fromList)

                    pinsBeforeInsideAndAfter =
                        window 100 25 (Set.fromList [ "1", "26", "50", "100" ])

                    ordinaryPage offset =
                        Cards.presentationWindow String.fromInt (Set.singleton "50") offset (List.range 1 100)
                            |> List.filter ((/=) 50)

                    everyOrdinaryCard =
                        [ ordinaryPage 0, ordinaryPage 24, ordinaryPage 48, ordinaryPage 72, ordinaryPage 96 ]
                            |> List.concat

                    unrelatedBranchPins =
                        Cards.presentationWindow identity (Set.singleton "outside-branch") 0 (List.range 1 50 |> List.map String.fromInt)

                    localBranchPin =
                        Cards.presentationWindow identity (Set.singleton "50") 0 (List.range 1 50 |> List.map String.fromInt)

                    overflowingPins =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 0 (List.range 1 50 |> List.map String.fromInt)

                    overflowingPinsNext =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 1 (List.range 1 50 |> List.map String.fromInt)

                    terminalPins =
                        Cards.presentationWindow identity (List.range 1 30 |> List.map String.fromInt |> Set.fromList) 0 (List.range 1 30 |> List.map String.fromInt)
                in
                Expect.all
                    [ \_ -> ( List.length first25, List.head first25, List.reverse first25 |> List.head ) |> Expect.equal ( 25, Just "1", Just "25" )
                    , \_ -> ( List.length final50, List.head final50, List.reverse final50 |> List.head ) |> Expect.equal ( 25, Just "26", Just "50" )
                    , \_ -> Expect.equal [ "51" ] final51
                    , \_ -> ( List.length middle75, List.head middle75, List.reverse middle75 |> List.head ) |> Expect.equal ( 25, Just "26", Just "50" )
                    , \_ -> Expect.equal [ "76", "77", "78", "79", "80" ] final80
                    , \_ -> ( List.length pinned75, List.member "75" pinned75, List.member "25" pinned75 ) |> Expect.equal ( 25, True, False )
                    , \_ -> ( List.length fullyPinned, List.head fullyPinned, List.reverse fullyPinned |> List.head ) |> Expect.equal ( 26, Just "26", Just "75" )
                    , \_ ->
                        Expect.equal
                            { before = True, insideStart = True, insideEnd = True, after = True, count = 25 }
                            { before = List.member "1" pinsBeforeInsideAndAfter
                            , insideStart = List.member "26" pinsBeforeInsideAndAfter
                            , insideEnd = List.member "50" pinsBeforeInsideAndAfter
                            , after = List.member "100" pinsBeforeInsideAndAfter
                            , count = List.length pinsBeforeInsideAndAfter
                            }
                    , \_ -> Expect.equal (List.filter ((/=) 50) (List.range 1 100)) everyOrdinaryCard
                    , \_ -> Expect.equal ( 25, Just "25" ) ( List.length unrelatedBranchPins, List.reverse unrelatedBranchPins |> List.head )
                    , \_ -> Expect.equal ( 25, Just "50" ) ( List.length localBranchPin, List.reverse localBranchPin |> List.head )
                    , \_ -> Expect.equal (List.range 1 31 |> List.map String.fromInt) overflowingPins
                    , \_ -> Expect.equal ((List.range 1 30 |> List.map String.fromInt) ++ [ "32" ]) overflowingPinsNext
                    , \_ -> Expect.equal (List.range 1 30 |> List.map String.fromInt) terminalPins
                    , \_ -> Expect.equal [ 25, 24, 1, 1, 1 ] (List.map Helpers.presentationOrdinaryCapacity [ 0, 1, 24, 25, 30 ])
                    ]
                    ()
        ]


workspaceId : String
workspaceId =
    "workspace-1"


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


flags : Flags
flags =
    { apiUrl = "https://api.example"
    , wsUrl = "wss://api.example"
    , sessionId = "session-1"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


url : Url.Url
url =
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }


project : String -> Api.ProjectCardSummary
project id =
    { id = id
    , workspaceId = workspaceId
    , parentId = Nothing
    , name = id
    , status = Api.ProjActive
    , priority = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directProjectCount = 0
    , directTaskCount = 0
    , hasChildren = True
    , readinessRollup = { openProjectCount = 0, inProgressTaskCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


task : String -> Maybe String -> Api.TaskCardSummary
task id parentId =
    { id = id
    , workspaceId = workspaceId
    , projectId = Nothing
    , parentId = parentId
    , title = id
    , status = Api.Todo
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directSubtaskCount = 0
    , hasChildren = False
    , readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


viewportReady : Model -> Model
viewportReady source =
    let
        ready = { source | auth = { status = AuthReady, mode = Just "test" } }
    in
    Cards.refreshViewport source ( ready, Cmd.none ) |> Tuple.first


rootSeed : List Api.ProjectCardSummary -> Model
rootSeed summaries =
    let
        prepared = DataLoading.prepareRootNavigationRequest (Just workspaceId) model
        seeded = DataLoading.mergeNavigationSummaries summaries [] prepared
        loading = seeded.dataLoading
    in
    { seeded | dataLoading = { loading | rootNavigationRequest = Maybe.map (\request -> { request | inFlight = False, succeeded = True, projectHasMore = True, projectCardCount = List.length summaries, taskHasMore = False, projectRequestPending = False, taskRequestPending = False }) loading.rootNavigationRequest } }


routed : Msg -> Model -> Model
routed msg source =
    case UpdateRouter.update msg source of
        Ok result -> Tuple.first result
        Err _ -> source


viewportEvent : Model -> Float -> List ( String, Float ) -> Maybe String -> List String -> Encode.Value
viewportEvent source top measurements target pins =
    let
        viewport = source.cards.viewport
    in
    Encode.object
        [ ( "workspace", Encode.string workspaceId ), ( "epoch", Encode.int source.sessionRequestEpoch )
        , ( "generation", Encode.int source.dataLoading.navigationGeneration ), ( "revision", Encode.int viewport.revision )
        , ( "top", Encode.float top ), ( "height", Encode.float 600 )
        , ( "measurements", Encode.list (\( key, amount ) -> Encode.object [ ( "key", Encode.string key ), ( "height", Encode.float amount ) ]) measurements )
        , ( "request", target |> Maybe.map Encode.string |> Maybe.withDefault Encode.null ), ( "pins", Encode.list Encode.string pins )
        ]


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


rootPaint : Model -> Model
rootPaint source =
    let
        prepared = DataLoading.prepareRootNavigationRequest (Just workspaceId) { source | auth = { status = AuthReady, mode = Just "test" } }
        response = { workspaceId = workspaceId, projects = { items = Dict.values source.dataLoading.projectCardSummaries |> List.filter (\summary -> summary.parentId == Nothing), hasMore = False }, tasks = { items = Dict.values source.dataLoading.taskCardSummaries |> List.filter (\summary -> summary.parentId == Nothing && summary.projectId == Nothing), hasMore = False } }
        loaded = case prepared.dataLoading.rootNavigationRequest of
            Just request -> DataLoading.update (GotRootNavigation workspaceId request.sessionEpoch prepared.dataLoading.activeWorkspaceLoadToken request.generation request.filterFingerprint 0 0 (Ok response)) prepared |> Cards.refreshViewport prepared |> Tuple.first
            Nothing -> prepared
        viewport = loaded.cards.viewport
    in
    Cards.updateViewport (Encode.object
        [ ( "workspace", Encode.string workspaceId ), ( "epoch", Encode.int loaded.sessionRequestEpoch ), ( "generation", Encode.int loaded.dataLoading.navigationGeneration ), ( "revision", Encode.int viewport.revision )
        , ( "paintNonce", Encode.int (DataLoading.backgroundPaintNonce loaded) ), ( "paintFilter", Encode.string (DataLoading.navigationFilterFingerprint loaded) )
        ]) loaded |> Tuple.first
