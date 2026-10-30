module MoveTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Move as Move
import Feature.DataLoading as Loading
import Html.Attributes as Attr
import Http
import Json.Encode as Encode
import Set
import StartupAdmission
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


suite : Test
suite =
    describe "shared Move chooser"
        [ test "independent default statuses and projects-only empty-project query" <| \_ ->
            let source = step (OpenMove "task" "child") base in
            Expect.equal ( Just "&show_only=projects&show_empty_projects=true&project_status=active&project_status=paused", base.search )
                ( source.move.dialog |> Maybe.map Move.filterQuery, source.search )
        , test "search retires logical reads without releasing physical slots or accepting delayed pages" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                oldId = readId "" opened
                changed = step (MoveQueryChanged "needle") opened
                after = step (MoveBranchFetched oldId (Ok (page [ project "stale" Nothing ] False))) changed
            in
            Expect.equal ( 4, False )
                ( Dict.size changed.move.admissions, after.move.dialog |> Maybe.map (.projects >> Dict.member "stale") |> Maybe.withDefault True )
        , test "close/reopen retains old admissions and rejects their success and errors" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                old = readId "" opened
                reopened = opened |> step CloseMove |> step (OpenMove "project" "source")
                settled = reopened |> step (MoveBranchFetched old (Err Http.Timeout))
            in
            Expect.equal ( 4, Nothing ) ( Dict.size reopened.move.admissions, settled.move.dialog |> Maybe.andThen (.branches >> Dict.get "" >> Maybe.andThen .error) )
        , test "shared physical budget blocks picker admission when main hierarchy owns four slots" <| \_ ->
            let
                sample = { workspaceId = "ws", readRevision = 0, sessionEpoch = 0, generation = 0, filterFingerprint = "", projectOffset = 0, taskOffset = 0, inFlight = True, succeeded = False, projectHasMore = False, taskHasMore = False, projectCardCount = 0, taskCardCount = 0, projectRequestPending = True, taskRequestPending = False }
                loading = base.dataLoading
                opened = step (OpenMove "task" "child") { base | dataLoading = { loading | navigationAdmissions = Dict.fromList (List.range 1 4 |> List.map (\n -> ( String.fromInt n, sample ))) } }
            in
            Expect.equal 0 (Dict.size opened.move.admissions)
        , test "picker counts an in-flight root in addition to three ordinary owner slots" <| \_ ->
            let
                sample = { workspaceId = "ws", readRevision = 0, sessionEpoch = 0, generation = 0, filterFingerprint = "", projectOffset = 0, taskOffset = 0, inFlight = True, succeeded = False, projectHasMore = False, taskHasMore = False, projectCardCount = 0, taskCardCount = 0, projectRequestPending = True, taskRequestPending = False }
                loading = base.dataLoading
                occupied = { base | dataLoading = { loading | navigationAdmissions = Dict.fromList (List.range 1 3 |> List.map (\n -> ( String.fromInt n, sample ))) } }
                rooted = { occupied | dataLoading = { loading | navigationAdmissions = occupied.dataLoading.navigationAdmissions, rootNavigationRequest = Just sample } }
                ordinary = step (OpenMove "task" "child") occupied
                blocked = step (OpenMove "task" "child") rooted
            in
            Expect.equal ( 1, 0 ) ( Dict.size ordinary.move.admissions, Dict.size blocked.move.admissions )
        , test "retry gets a fresh read identity and reports malformed nonprogress as incomplete" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                old = readId "" opened
                failed = step (MoveBranchFetched old (Ok (page [] True))) opened
                retried = step (MoveRetry "") failed
            in
            Expect.equal ( True, True )
                ( failed.move.dialog |> Maybe.andThen (.branches >> Dict.get "" >> Maybe.andThen .error) |> (/=) Nothing
                , readId "" retried > old )
        , test "expanded branch loads pages; collapse retires delayed child pages" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> loadRoot [ project "destination" Nothing ]
                expanded = step (MoveToggleBranch "destination") loaded
                request = readId "destination" expanded
                collapsed = step (MoveToggleBranch "destination") expanded
                late = step (MoveBranchFetched request (Ok (page [ project "nested" (Just "destination") ] False))) collapsed
            in
            Expect.equal ( False, False ) ( late.move.dialog |> Maybe.map (.projects >> Dict.member "nested") |> Maybe.withDefault True, late.move.dialog |> Maybe.map (.expanded >> Set.member "destination") |> Maybe.withDefault True )
        , test "same placement, self and descendant destinations cannot be selected" <| \_ ->
            let
                loaded = step (OpenMove "project" "source") base |> loadRoot [ project "source" Nothing, project "destination" Nothing ]
                enriched = mapDialog (\d -> { d | projects = Dict.insert "descendant" (project "descendant" (Just "source")) d.projects }) loaded
                reasons = enriched.move.dialog |> Maybe.map (\d -> List.map (Move.destinationReason enriched d) [ Nothing, Just "source", Just "descendant" ]) |> Maybe.withDefault []
            in
            Expect.equal [ Just "This is the current location.", Just "A project cannot move into itself or its descendants.", Just "A project cannot move into itself or its descendants." ] reasons
        , test "blocked parent with authoritative running child cannot move into paused ancestry" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> loadRoot [ pausedProject ]
                dependencies = loaded.dependencies
                rollup = { openSubtaskCount = 1, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = False, inProgressSubtaskCount = 1, canStartSubtasks = False, ownOpenDependencyCount = Just 0 }
                running = { loaded | dependencies = { dependencies | taskReadinessRollups = Dict.singleton "child" rollup } }
            in
            Expect.equal (Just "Running work cannot move into paused ancestry.") (running.move.dialog |> Maybe.andThen (\d -> Move.destinationReason running d (Just "paused")))
        , test "nonmatching ancestor context is disabled while description-only match becomes selectable" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> step (MoveQueryChanged "needle") |> loadRoot [ project "destination" Nothing ]
                read = readId "@match:destination" loaded
                full = Api.projectFromCardSummary (project "destination" Nothing)
                matched = step (MoveMatchFetched read (Ok { full | description = Just "needle in description" })) loaded
            in
            Expect.equal Nothing (matched.move.dialog |> Maybe.andThen (\d -> Move.destinationReason matched d (Just "destination")))
        , test "subtask payload only changes project and explicitly clears task parent" <| \_ ->
            let source = ready in
            Expect.equal (Just "{\"project_id\":\"destination\",\"parent_id\":null}") (source.move.dialog |> Maybe.map (\d -> Move.moveFields d (Just "destination") |> Encode.object |> Encode.encode 0))
        , test "save authority is exact; errors preserve selection and saving prevents duplicates" <| \_ ->
            let
                saving = step MoveConfirm (step (MoveSelect (Just "destination")) ready)
                duplicate = step MoveConfirm saving
                dialog = saving.move.dialog
                failed = dialog |> Maybe.andThen (\d -> Dict.values saving.mutations.taskAcknowledgements |> List.head |> Maybe.map (\operation -> step (MoveTaskSaved d.token d.guard operation (Err (Api.decodeApiErrorBody 409 "{\"error\":\"lifecycle_conflict\",\"message\":\"Blocked\"}"))) saving)) |> Maybe.withDefault saving
            in
            Expect.equal ( saving.mutations.nextRequestId, Just (Just "destination"), ( Nothing, True ) )
                ( duplicate.mutations.nextRequestId, failed.move.dialog |> Maybe.andThen .selected, ( failed.move.dialog |> Maybe.andThen .saving, failed.move.dialog |> Maybe.andThen .error |> (/=) Nothing ) )
        , test "canonical success applies exact task placement and closes; late close callback cannot change data" <| \_ ->
            let
                saving = step MoveConfirm (step (MoveSelect (Just "destination")) ready)
                moved = { child | projectId = Just "destination", parentId = Nothing }
                apply source = saving.move.dialog |> Maybe.andThen (\d -> Dict.values saving.mutations.taskAcknowledgements |> List.head |> Maybe.map (\operation -> step (MoveTaskSaved d.token d.guard operation (Ok { task = moved, dependencyEffects = [] })) source)) |> Maybe.withDefault source
                accepted = apply saving
                retired = apply (step CloseMove saving)
            in
            Expect.equal ( Nothing, Just Nothing, Just (Just "parent") )
                ( accepted.move.dialog, Dict.get "child" accepted.tasks |> Maybe.map .parentId, Dict.get "child" retired.tasks |> Maybe.map .parentId )
        , test "authorization retirement closes and retains physical occupancy for completion" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                retired = Move.reconcile ( { opened | sessionRequestEpoch = opened.sessionRequestEpoch + 1 }, Cmd.none ) |> Tuple.first
            in
            Expect.equal ( Nothing, 2 ) ( retired.move.dialog, Dict.size retired.move.admissions )
        , test "one released main navigation slot admits picker before ordinary main continuation" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                move = opened.move
                waiting = { opened | move = { move | admissions = Dict.empty, dialog = move.dialog |> Maybe.map (\d -> { d | focusPending = Nothing, branches = Dict.empty }) } }
                loading = waiting.dataLoading
                sample = { workspaceId = "ws", readRevision = 0, sessionEpoch = 0, generation = 0, filterFingerprint = "", projectOffset = 0, taskOffset = 0, inFlight = True, succeeded = False, projectHasMore = False, taskHasMore = False, projectCardCount = 0, taskCardCount = 0, projectRequestPending = True, taskRequestPending = False }
                occupied = { waiting | dataLoading = { loading | navigationAdmissions = Dict.fromList (List.range 1 4 |> List.map (\n -> ( String.fromInt n, sample ))) } }
                full = Move.reconcile ( occupied, Cmd.none ) |> Tuple.first
                releasedLoading = full.dataLoading
                released = Move.reconcile ( { full | dataLoading = { releasedLoading | navigationAdmissions = Dict.remove "1" releasedLoading.navigationAdmissions } }, Cmd.none ) |> Tuple.first
            in
            Expect.equal ( 0, 1, 3 ) ( Dict.size full.move.admissions, Dict.size released.move.admissions, Dict.size released.dataLoading.navigationAdmissions )
        , test "six main details block match lookup; one released slot admits it and close retains its occupancy" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base |> step (MoveQueryChanged "needle")
                loading = opened.dataLoading
                occupied = { opened | dataLoading = { loading | cardDetailAdmissions = Set.fromList (List.range 1 6) } }
                loaded = loadRoot [ project "destination" Nothing ] occupied
                releasedLoading = loaded.dataLoading
                released = Move.reconcile ( { loaded | dataLoading = { releasedLoading | cardDetailAdmissions = Set.remove 1 releasedLoading.cardDetailAdmissions } }, Cmd.none ) |> Tuple.first
                matchId = readId "@match:destination" released
                closed = step CloseMove released
                completed = step (MoveMatchFetched matchId (Ok (Api.projectFromCardSummary (project "destination" Nothing)))) closed
            in
            Expect.equal ( -1, True, 1 )
                ( readId "@match:destination" loaded, matchId > 0, Dict.size closed.move.admissions - Dict.size completed.move.admissions )
        , test "late old-query description match cannot enable a current-query candidate" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> step (MoveQueryChanged "needle") |> loadRoot [ project "destination" Nothing ]
                old = readId "@match:destination" loaded
                changed = step (MoveQueryChanged "another") loaded
                full = Api.projectFromCardSummary (project "destination" Nothing)
                after = step (MoveMatchFetched old (Ok { full | description = Just "needle" })) changed
            in
            Expect.equal False (after.move.dialog |> Maybe.map (.matches >> Dict.member "destination") |> Maybe.withDefault True)
        , test "close resumes queued main navigation and focused detail demand without changing ordinary scheduling" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base |> Loading.syncPlanningCounts |> Tuple.first
                loading = opened.dataLoading
                startup = loading.startupAdmission
                permit = loading.backgroundAdmission
                focused = opened.focus
                cards = opened.cards
                queued = Loading.beginNavigationBranch "project" "ws" (Just "destination")
                    { opened | focus = { focused | focusedEntity = Just ( "project", "destination" ) }
                        , cards = { cards | collapsedNodes = Dict.insert "proj-destination" False cards.collapsedNodes }
                        , dataLoading = { loading | projectCardSummaries = Dict.singleton "destination" (project "destination" Nothing), startupAdmission = { startup | authorized = True, phase = StartupAdmission.Normal, initialWaveOpened = True }, backgroundAdmission = { permit | workspaceId = Just "ws", sessionEpoch = opened.sessionRequestEpoch, filterFingerprint = Loading.navigationFilterFingerprint opened, rootGeneration = Nothing, remaining = 6, acknowledged = True }, navigationVisibilityActive = False } }
                    |> Tuple.first
                resumed = Move.reconcile (step CloseMove queued, Cmd.none) |> Tuple.first
            in
            Expect.equal ( 0, 1, True )
                ( Dict.size queued.dataLoading.navigationAdmissions, Dict.size resumed.dataLoading.navigationAdmissions, Dict.get "destination" resumed.dataLoading.projectCardDetailRequests |> Maybe.map .inFlight |> Maybe.withDefault False )
                |> Expect.onFail (Encode.encode 0 (Encode.object [ ( "queue", Encode.list Encode.string resumed.dataLoading.navigationQueue ), ( "remaining", Encode.int resumed.dataLoading.backgroundAdmission.remaining ), ( "startupAuthorized", Encode.bool resumed.dataLoading.startupAdmission.authorized ), ( "ordinary", Encode.bool (StartupAdmission.canOrdinary resumed.dataLoading.startupAdmission) ) ]))
        , test "fresh focus target replaces cached project/parent/readiness before confirmation" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                rollup = { openSubtaskCount = 1, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = False, inProgressSubtaskCount = 1, canStartSubtasks = False, ownOpenDependencyCount = Just 0 }
                target = { id = "child", workspaceId = "ws", projectId = Just "destination", parentId = Nothing, title = "Fresh child", status = Api.Blocked, priority = 5, completedAt = Nothing, dependencyCount = 0, createdAt = "now", updatedAt = "newer", directSubtaskCount = 1, hasChildren = True, readinessRollup = rollup }
                fresh = step (MoveFocusFetched (readId "@focus" opened) (Ok { workspaceId = "ws", target = Api.NavigationTaskSummary target, ancestors = [ Api.NavigationProjectSummary (project "destination" Nothing) ], ancestorsTruncated = False, nextAncestorOffset = Nothing })) opened
            in
            Expect.equal (Just ( Just "destination", Nothing, ( "Fresh child", True ) ))
                (fresh.move.dialog |> Maybe.map (\d -> ( d.currentProject, d.taskParent, ( d.name, d.sourceRunning ) )))
        , test "ancestor-page source drift refuses confirmation and Retry starts a fresh coherent page-zero chain" <| \_ ->
            let
                opened = step (OpenMove "project" "source") base
                original = project "source" Nothing
                changed = project "source" (Just "destination")
                response target truncated offset = { workspaceId = "ws", target = Api.NavigationProjectSummary target, ancestors = [], ancestorsTruncated = truncated, nextAncestorOffset = offset }
                pageOne = step (MoveFocusFetched (readId "@focus" opened) (Ok (response original True (Just 64)))) opened
                pageTwo = step (MoveFocusFetched (readId "@focus" pageOne) (Ok (response changed False Nothing))) pageOne
                retry = step (MoveRetry "@focus") pageTwo
                freshRead = readId "@focus" retry
                fresh = step (MoveFocusFetched freshRead (Ok { workspaceId = "ws", target = Api.NavigationProjectSummary changed, ancestors = [ Api.NavigationProjectSummary (project "destination" Nothing) ], ancestorsTruncated = False, nextAncestorOffset = Nothing })) retry
            in
            Expect.equal ( True, Just 0, Just ( Just "destination", True ) )
                ( pageTwo.move.dialog |> Maybe.andThen .focusError |> (/=) Nothing
                , Dict.get freshRead retry.move.admissions |> Maybe.map .offset
                , fresh.move.dialog |> Maybe.map (\d -> ( d.currentProject, d.focusComplete )) )
        , test "canonical focus unfolds deep current ancestry even outside destination statuses" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base
                parent = project "source" (Just "ancestor")
                ancestor = project "ancestor" Nothing
                response = { workspaceId = "ws", target = Api.NavigationTaskSummary { id = "child", workspaceId = "ws", projectId = Just "source", parentId = Nothing, title = "Child", status = Api.Todo, priority = 5, completedAt = Nothing, dependencyCount = 0, createdAt = "now", updatedAt = "now", directSubtaskCount = 0, hasChildren = False, readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True, inProgressSubtaskCount = 0, canStartSubtasks = True, ownOpenDependencyCount = Just 0 } }, ancestors = [ Api.NavigationProjectSummary { ancestor | status = Api.ProjCompleted }, Api.NavigationProjectSummary parent ], ancestorsTruncated = False, nextAncestorOffset = Nothing }
                focused = step (MoveFocusFetched (readId "@focus" opened) (Ok response)) opened
            in
            Expect.equal (Just ( [ ( "ancestor", 0 ), ( "source", 1 ) ], True, True ))
                (focused.move.dialog |> Maybe.map (\d -> ( Move.visibleRows d, Set.member "ancestor" d.expanded, Set.member "source" d.expanded )))
        , test "live revalidation preserves full displayed membership until exhaustion and clears a removed selection" <| \_ ->
            let
                selected = ready |> step (MoveSelect (Just "destination")) |> mapDialog (\d -> { d | refreshRequested = True })
                refreshed = Move.reconcile ( selected, Cmd.none ) |> Tuple.first
                firstPage = step (MoveBranchFetched (readId "" refreshed) (Ok (page (List.range 1 50 |> List.map (\n -> project (String.fromInt n) Nothing)) True))) refreshed
                finished = step (MoveBranchFetched (readId "" firstPage) (Ok (page [ project "last" Nothing ] False))) firstPage
                focused = step (MoveFocusFetched (readId "@focus" finished) (Ok childFocus)) finished
                settled = step (MoveBranchFetched (readId "source" focused) (Ok (page [] False))) focused
                    |> (\current -> step (MoveFocusFetched (readId "@selection" current) (Err (Http.BadStatus 404))) current)
            in
            Expect.equal ( Just [ "source", "destination" ], Just (Just "destination"), ( Nothing, Just 51 ) )
                ( firstPage.move.dialog |> Maybe.andThen (.branches >> Dict.get "" >> Maybe.map .ids)
                , firstPage.move.dialog |> Maybe.andThen .selected
                , ( settled.move.dialog |> Maybe.andThen .selected
                  , finished.move.dialog |> Maybe.andThen (.branches >> Dict.get "" >> Maybe.map (.ids >> List.length)) ) )
        , test "live refresh generations fence delayed pre-refresh membership while retaining valid selection" <| \_ ->
            let
                selected = ready |> step (MoveSelect (Just "destination"))
                old = readId "@focus" selected
                refreshed = Move.reconcile (mapDialog (\d -> { d | refreshRequested = True }) selected, Cmd.none) |> Tuple.first
                loaded = loadRoot [ project "source" Nothing, project "destination" Nothing, project "new" Nothing ] refreshed
                delayed = step (MoveFocusFetched old (Err Http.Timeout)) loaded
            in
            Expect.equal ( Just (Just "destination"), Nothing, True )
                ( delayed.move.dialog |> Maybe.andThen .selected
                , delayed.move.dialog |> Maybe.andThen .focusError
                , delayed.move.dialog |> Maybe.map (.projects >> Dict.member "new") |> Maybe.withDefault False )
        , test "rechecking a description-only match preserves selection until its fresh result" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> step (MoveQueryChanged "needle") |> loadRoot [ project "destination" Nothing ]
                full = Api.projectFromCardSummary (project "destination" Nothing)
                matched = step (MoveMatchFetched (readId "@match:destination" loaded) (Ok { full | description = Just "needle description" })) loaded
                selected = step (MoveSelect (Just "destination")) matched
                refreshing = Move.reconcile (mapDialog (\d -> { d | refreshRequested = True }) selected, Cmd.none) |> Tuple.first
                currentGeneration = refreshing.move.dialog |> Maybe.map .generation |> Maybe.withDefault -1
                drained = Dict.toList refreshing.move.admissions |> List.filter (Tuple.second >> .generation >> (/=) currentGeneration)
                    |> List.foldl (\( id, _ ) current -> step (MoveBranchFetched id (Err Http.Timeout)) current) refreshing
                reloaded = loadRoot [ project "destination" Nothing ] drained
                rechecked = step (MoveMatchFetched (readId "@match:destination" reloaded) (Ok { full | description = Just "needle description" })) reloaded
            in
            Expect.equal ( Just (Just "destination"), Just (Just "destination") )
                ( reloaded.move.dialog |> Maybe.andThen .selected
                , rechecked.move.dialog |> Maybe.andThen .selected )
        , test "selected non-context destination survives new-parent completion before old-parent completion" <| \_ ->
            checkReparentRace True
        , test "selected non-context destination survives old-parent completion before new-parent completion" <| \_ ->
            checkReparentRace False
        , test "empty status membership settles after live focus refresh and permits root confirmation" <| \_ ->
            let
                empty = ready |> step (MoveStatusChanged "active") |> step (MoveStatusChanged "paused")
                selected = empty |> step (MoveSelect Nothing) |> mapDialog (\d -> { d | refreshRequested = True })
                refreshing = Move.reconcile ( selected, Cmd.none ) |> Tuple.first |> drainRetiredReads
                settled = step (MoveFocusFetched (readId "@focus" refreshing) (Ok childFocus)) refreshing
                confirmed = step MoveConfirm settled
            in
            Expect.equal ( Just ( True, False, Just Nothing ), True )
                ( settled.move.dialog |> Maybe.map (\d -> ( d.focusComplete, d.revalidating, d.selected ))
                , confirmed.move.dialog |> Maybe.andThen .saving |> (/=) Nothing )
        , test "canonical selected path preserves a destination moved below a collapsed unloaded parent" <| \_ ->
            let
                opened = step (OpenMove "task" "child") base |> loadRoot [ project "source" Nothing, project "a" Nothing, project "b" Nothing ] |> mapDialog (\d -> { d | focusComplete = True })
                expanded = step (MoveToggleBranch "a") opened
                loaded = step (MoveBranchFetched (readId "a" expanded) (Ok (page [ project "chosen" (Just "a") ] False))) expanded
                selected = step (MoveSelect (Just "chosen")) loaded |> mapDialog (\d -> { d | refreshRequested = True })
                refreshed = Move.reconcile ( selected, Cmd.none ) |> Tuple.first |> drainRetiredReads
                    |> loadRoot [ project "source" Nothing, project "a" Nothing, project "b" Nothing ]
                focused = step (MoveFocusFetched (readId "@focus" refreshed) (Ok childFocus)) refreshed
                oldFinished = step (MoveBranchFetched (readId "a" focused) (Ok (page [] False))) focused
                    |> (\current -> step (MoveBranchFetched (readId "source" current) (Ok (page [] False))) current)
                canonical = step (MoveFocusFetched (readId "@selection" oldFinished) (Ok (projectFocus (project "chosen" (Just "b")) [ project "b" Nothing ]))) oldFinished
                settled = step (MoveBranchFetched (readId "b" canonical) (Ok (page [ project "chosen" (Just "b") ] False))) canonical
                confirmed = step MoveConfirm settled
            in
            Expect.equal ( Just (Just "chosen"), Just ( True, False, True ), True )
                ( oldFinished.move.dialog |> Maybe.andThen .selected
                , settled.move.dialog |> Maybe.map (\d -> ( Set.member "b" d.expanded, d.revalidating, List.member ( "chosen", 1 ) (Move.visibleRows d) ))
                , confirmed.move.dialog |> Maybe.andThen .saving |> (/=) Nothing )
        , test "selected destination validation retries and rejects stale or foreign focus replies" <| \_ ->
            let
                selected = ready |> step (MoveSelect (Just "destination")) |> mapDialog (\d -> { d | refreshRequested = True })
                refreshed = Move.reconcile ( selected, Cmd.none ) |> Tuple.first |> drainRetiredReads
                old = readId "@selection" refreshed
                failed = step (MoveFocusFetched old (Ok (projectFocus (project "foreign" Nothing) []))) refreshed
                retry = step (MoveRetry "@selection") failed
                new = readId "@selection" retry
                late = step (MoveFocusFetched old (Ok (projectFocus (project "destination" Nothing) []))) retry
                accepted = step (MoveFocusFetched new (Ok (projectFocus (project "destination" Nothing) []))) late
                deleted = step (MoveFocusFetched old (Err (Http.BadStatus 404))) refreshed
            in
            Expect.equal ( True, Just False, ( Just True, Nothing ) )
                ( new > old
                , late.move.dialog |> Maybe.andThen .selectionValidation |> Maybe.map .complete
                , ( accepted.move.dialog |> Maybe.andThen .selectionValidation |> Maybe.map .complete, deleted.move.dialog |> Maybe.andThen .selected ) )
        , test "switching to root retires held destination validation without blocking a ready source" <| \_ ->
            let
                selected = ready |> step (MoveSelect (Just "destination")) |> mapDialog (\d -> { d | refreshRequested = True })
                refreshed = Move.reconcile ( selected, Cmd.none ) |> Tuple.first |> drainRetiredReads |> loadRoot [ project "source" Nothing, project "destination" Nothing ]
                focused = step (MoveFocusFetched (readId "@focus" refreshed) (Ok childFocus)) refreshed
                loaded = step (MoveBranchFetched (readId "source" focused) (Ok (page [] False))) focused
                old = readId "@selection" loaded
                root = step (MoveSelect Nothing) loaded
                delayed = step (MoveFocusFetched old (Err (Http.BadStatus 404))) root
                confirmed = step MoveConfirm delayed
            in
            Expect.equal ( Just Nothing, True )
                ( delayed.move.dialog |> Maybe.andThen .selected
                , confirmed.move.dialog |> Maybe.andThen .saving |> (/=) Nothing )
        , test "view mounts 25 ordinary rows plus root with keyboard-accessible separate expand actions" <| \_ ->
            let
                loaded = step (OpenMove "task" "child") base |> loadRoot (List.range 1 50 |> List.map (\n -> project (String.fromInt n) Nothing))
            in
            Move.view loaded |> Query.fromHtml |> Query.findAll [ Selector.class "move-project-row" ] |> Query.count (Expect.equal 26)
        ]


checkReparentRace : Bool -> Expect.Expectation
checkReparentRace newParentFirst =
    let
        roots = [ project "source" Nothing, project "a" Nothing, project "b" Nothing ]
        opened = step (OpenMove "task" "child") base |> loadRoot roots |> mapDialog (\d -> { d | focusComplete = True })
        oldParent = step (MoveToggleBranch "a") opened
        oldLoaded = step (MoveBranchFetched (readId "a" oldParent) (Ok (page [ project "chosen" (Just "a") ] False))) oldParent
        newParent = step (MoveToggleBranch "b") oldLoaded
        newLoaded = step (MoveBranchFetched (readId "b" newParent) (Ok (page [] False))) newParent
        selected = step (MoveSelect (Just "chosen")) newLoaded |> mapDialog (\d -> { d | refreshRequested = True })
        refreshed = Move.reconcile ( selected, Cmd.none ) |> Tuple.first |> drainRetiredReads |> loadRoot roots
        coherent = step (MoveFocusFetched (readId "@focus" refreshed) (Ok childFocus)) refreshed
        finishOld model = step (MoveBranchFetched (readId "a" model) (Ok (page [] False))) model
        finishNew model = step (MoveBranchFetched (readId "b" model) (Ok (page [ project "chosen" (Just "b") ] False))) model
        partial = if newParentFirst then finishNew coherent else finishOld coherent
        final = if newParentFirst then finishOld partial else finishNew partial
    in
    Expect.equal ( Just (Just "chosen"), Just ( Just "b", Just (Just "chosen"), ( True, False ) ) )
        ( partial.move.dialog |> Maybe.andThen .selected
        , final.move.dialog |> Maybe.map (\d -> ( Dict.get "chosen" d.projects |> Maybe.andThen .parentId, d.selected
            , ( List.member ( "chosen", 1 ) (Move.visibleRows d), Set.member "chosen" d.contextIds ) )) )


drainRetiredReads : Model -> Model
drainRetiredReads model =
    let generation = model.move.dialog |> Maybe.map .generation |> Maybe.withDefault -1 in
    Dict.toList model.move.admissions |> List.filter (Tuple.second >> .generation >> (/=) generation)
        |> List.foldl (\( id, _ ) current -> step (MoveBranchFetched id (Err Http.Timeout)) current) model


projectFocus : Api.ProjectCardSummary -> List Api.ProjectCardSummary -> Api.NavigationFocusResponse
projectFocus target ancestors =
    { workspaceId = "ws", target = Api.NavigationProjectSummary target, ancestors = List.map Api.NavigationProjectSummary ancestors, ancestorsTruncated = False, nextAncestorOffset = Nothing }


childFocus : Api.NavigationFocusResponse
childFocus =
    { workspaceId = "ws", target = Api.NavigationTaskSummary
        { id = "child", workspaceId = "ws", projectId = Just "source", parentId = Nothing, title = "Child", status = Api.Todo, priority = 5, completedAt = Nothing, dependencyCount = 0, createdAt = "now", updatedAt = "now", directSubtaskCount = 0, hasChildren = False
        , readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True, inProgressSubtaskCount = 0, canStartSubtasks = True, ownOpenDependencyCount = Just 0 } }
    , ancestors = [ Api.NavigationProjectSummary (project "source" Nothing) ], ancestorsTruncated = False, nextAncestorOffset = Nothing }


step : Msg -> Model -> Model
step msg model = Move.update msg model |> Tuple.first


readId : String -> Model -> Int
readId branch model = Dict.toList model.move.admissions |> List.filter (Tuple.second >> .branch >> (==) branch) |> List.reverse |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault -1


mapDialog : (MoveDialog -> MoveDialog) -> Model -> Model
mapDialog fn model = let state = model.move in { model | move = { state | dialog = Maybe.map fn state.dialog } }


loadRoot : List Api.ProjectCardSummary -> Model -> Model
loadRoot projects model = step (MoveBranchFetched (readId "" model) (Ok (page projects False))) model


page : List Api.ProjectCardSummary -> Bool -> Api.NavigationBranchResponse
page projects hasMore = { workspaceId = "ws", projects = { items = projects, hasMore = hasMore }, tasks = { items = [], hasMore = False } }


ready : Model
ready =
    step (OpenMove "task" "child") base |> loadRoot [ project "source" Nothing, project "destination" Nothing ]
        |> mapDialog (\d -> { d | focusComplete = True })


base : Model
base =
    let initial = AppShell.initModel Nothing
            { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/ws", query = Nothing, fragment = Nothing }
            (WorkspacePage "ws")
            { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session", runtimeMode = "local", authTokenStorageKey = "token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
            Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
    in
    { initial | selectedWorkspaceId = Just "ws", auth = { status = AuthReady, mode = Just "local" }
        , sessionContext = Just { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "local", actorLabel = "Local", authority = "local_superadmin", grantUserId = Nothing }, globalPermissions = { createWorkspace = True, superadmin = True }, workspace = Nothing }
        , projects = [ project "source" Nothing, project "destination" Nothing ] |> List.map (\p -> ( p.id, Api.projectFromCardSummary p )) |> Dict.fromList
        , tasks = Dict.singleton "child" child
    }


child : Api.Task
child = { id = "child", workspaceId = "ws", projectId = Just "source", parentId = Just "parent", title = "Child", description = Nothing, status = Api.Blocked, priority = 5, completedAt = Nothing, dependencyCount = 0, memoryLinkCount = 0, createdAt = "now", updatedAt = "now" }


project : String -> Maybe String -> Api.ProjectCardSummary
project id parent =
    { id = id, workspaceId = "ws", parentId = parent, name = id, status = Api.ProjActive, priority = 5, createdAt = "now", updatedAt = "now"
    , directProjectCount = 1, directTaskCount = 0, hasChildren = True
    , readinessRollup = { openProjectCount = 0, inProgressTaskCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }


pausedProject : Api.ProjectCardSummary
pausedProject = let p = project "paused" Nothing in { p | status = Api.ProjPaused }
