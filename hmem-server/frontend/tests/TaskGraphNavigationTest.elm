module TaskGraphNavigationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.FocusedFilters as Filters
import Feature.DataLoading as Loading
import Feature.TaskGraphNavigation as Navigation
import Json.Encode as Encode
import Set
import TaskGraphNavigation as History
import Test exposing (Test, describe, test)
import Types exposing (..)
import Url


suite : Test
suite =
    describe "task graph navigation origins"
        [ test "routes preserve opaque identifiers and reject duplicate graph targets" <| \_ ->
            let
                graph = History.graphUrl "project:one" "runtime:entry:1" url |> Url.fromString |> Maybe.andThen History.parse
                duplicate = { url | fragment = Just "task-graph=one&task-graph=two" }
            in Expect.equal ( Just { workspaceId = "w", projectId = "project:one", entryId = Just "runtime:entry:1" }, Nothing ) ( graph, History.parse duplicate )
        , test "each history entry restores its own focus, query, filters and expansion without restoring entity caches" <| \_ ->
            let
                originA = configure Nothing "alpha" 180 base
                ( graphA, idA ) = enter originA
                sourceA = source idA graphA
                originB = configure (Just ( "project", "p" )) "beta" 420 sourceA
                ( graphB, idB ) = enter originB
                restoredA = source idA graphB
                restoredB = source idB restoredA
                facts model = { focus = model.focus.focusedEntity, filters = Filters.capture model.search
                    , collapsed = model.cards.collapsedNodes, expanded = model.cards.expandedCards
                    , viewportTop = model.cards.viewport.top, scrollTop = model.mainContentScrollY }
            in
            Expect.all
                [ \_ -> Expect.notEqual idA idB
                , \_ -> Expect.equal (facts originA) (facts restoredA)
                , \_ -> Expect.equal (facts originB) (facts restoredB)
                , \_ -> Expect.equal graphB.projects restoredA.projects
                , \_ -> Expect.equal (Dict.size graphB.taskGraphNavigation.history.entries) 2
                ] ()
        , test "a browser-forward graph route keeps its independent entry identity" <| \_ ->
            let
                ( graphA, idA ) = enter base
                ( graphB, idB ) = enter (configure Nothing "second" 210 (source idA graphA))
                entryA = Dict.get idA graphB.taskGraphNavigation.history.entries
                forwarded = entryA |> Maybe.andThen (.graphUrl >> Url.fromString) |> Maybe.andThen (\target -> Navigation.handleUrl target graphB) |> Maybe.map Tuple.first
            in Expect.equal (Just (Just idA), True) (forwarded |> Maybe.andThen (.taskGraphNavigation >> .active) |> Maybe.map .entryId, idA /= idB)
        , test "scope retirement clears private origins and rejects late capture callbacks" <| \_ ->
            let
                opened = Navigation.update (OpenTaskGraph "p") base |> Tuple.first
                changed = { opened | sessionRequestEpoch = opened.sessionRequestEpoch + 1 }
                late = Navigation.update (TaskGraphNavigationEvent (captureEvent opened)) changed |> Tuple.first
                retired = Navigation.reconcile opened (late, Cmd.none) |> Tuple.first
            in Expect.equal (0,Nothing) (Dict.size retired.taskGraphNavigation.history.entries,retired.taskGraphNavigation.pending)
        , test "a late restoration acknowledgement cannot retire a newer visit of the same entry" <| \_ ->
            let
                ( graph, id ) = enter base
                first = source id graph
                entry = Dict.get id graph.taskGraphNavigation.history.entries
                second = entry |> Maybe.map (\value -> Navigation.restore id value first |> Tuple.first) |> Maybe.withDefault first
                oldToken = first.taskGraphNavigation.returning |> Maybe.map .token |> Maybe.withDefault -1
                acknowledgement = Encode.object [ ("kind",Encode.string "restored"),("id",Encode.string id),("token",Encode.int oldToken)
                    ,("scope",Encode.object [("workspace",Encode.string "w"),("epoch",Encode.int 1),("audience",Encode.string "reader")]) ]
                after = Navigation.update (TaskGraphNavigationEvent acknowledgement) second |> Tuple.first
            in Expect.equal (Just (oldToken + 1)) (after.taskGraphNavigation.returning |> Maybe.map .token)
        , test "rapid opens reserve distinct identities before DOM capture arrives" <| \_ ->
            let
                first = Navigation.update (OpenTaskGraph "p") base |> Tuple.first
                second = Navigation.update (OpenTaskGraph "p") first |> Tuple.first
            in Expect.notEqual (first.taskGraphNavigation.pending |> Maybe.map .id) (second.taskGraphNavigation.pending |> Maybe.map .id)
        , test "real tab navigation retires return ownership before bridge admission" <| \_ ->
            let
                (graph,id)=enter base
                returned=source id graph
                cards=returned.cards
                viewport=cards.viewport
                pinned={returned|cards={cards|viewport={viewport|nativePins=Set.fromList ["project:p","task:user-focus"]}}}
                away=AppShell.prepareOwned (AppShell.SwitchTabMsg SearchTab) pinned |> Navigation.reconcile pinned |> Tuple.first
                reentered=AppShell.prepareOwned (AppShell.SwitchTabMsg ProjectsTab) away |> Navigation.reconcile away |> Tuple.first
            in Expect.equal (Nothing,Nothing,Set.singleton "task:user-focus") (reentered.taskGraphNavigation.returning,reentered.taskGraphNavigation.returnNotice,reentered.cards.viewport.nativePins)
        , test "unrestored return is truthful, cancellation silent and scope retirement clears notice" <| \_ ->
            let
                (graph,id)=enter (configure (Just ("project","p")) "query" 180 base)
                returned=source id graph
                cards=returned.cards
                viewport=cards.viewport
                pinned={returned|cards={cards|viewport={viewport|nativePins=Set.fromList ["project:p","task:user-focus"]}}}
                outcome name target = Navigation.update (TaskGraphNavigationEvent (Encode.object [("kind",Encode.string "restored"),("id",Encode.string id),("token",Encode.int (target.taskGraphNavigation.returning |> Maybe.map .token |> Maybe.withDefault -1)),("outcome",Encode.string name)
                    ,("scope",Encode.object [("workspace",Encode.string "w"),("epoch",Encode.int 1),("audience",Encode.string "reader")])])) target |> Tuple.first
                unavailable=outcome "unrestored" pinned
                malformed=Navigation.update (TaskGraphNavigationEvent (Encode.object [("kind",Encode.string "restored"),("scope",Encode.object [("workspace",Encode.string "w"),("epoch",Encode.int 1),("audience",Encode.string "reader")])])) unavailable |> Tuple.first
                again=Dict.get id graph.taskGraphNavigation.history.entries |> Maybe.map (\entry -> Navigation.restore id entry unavailable |> Tuple.first) |> Maybe.withDefault unavailable
                cancelled=outcome "cancelled" again
                retired=Navigation.reconcile unavailable ({unavailable|sessionRequestEpoch=2},Cmd.none) |> Tuple.first
            in Expect.all [\_ -> Expect.notEqual Nothing unavailable.taskGraphNavigation.returnNotice,\_ -> Expect.equal (Set.singleton "task:user-focus") unavailable.cards.viewport.nativePins
                ,\_ -> Expect.equal returned.focus unavailable.focus,\_ -> Expect.equal returned.search.query unavailable.search.query
                ,\_ -> Expect.equal unavailable.taskGraphNavigation.returnNotice malformed.taskGraphNavigation.returnNotice
                ,\_ -> Expect.equal Nothing again.taskGraphNavigation.returnNotice,\_ -> Expect.equal Nothing cancelled.taskGraphNavigation.returnNotice,\_ -> Expect.equal Nothing retired.taskGraphNavigation.returnNotice] ()
        , test "a readable current focused target is ready despite a failed unrelated root request" <| \_ ->
            let
                ( graph, id ) = enter (configure (Just ("project","p")) "" 180 base)
                returned = source id graph
                prepared = Loading.prepareRootNavigationRequest (Just "w") returned
                loading = prepared.dataLoading
                cards = prepared.cards
                viewport = cards.viewport
                readable = { prepared | dataLoading = { loading | rootNavigationRequest = Maybe.map (\request -> { request | inFlight = False, succeeded = False }) loading.rootNavigationRequest }
                    , cards = { cards | viewport = { viewport | workspaceId = Just "w", sessionEpoch = prepared.sessionRequestEpoch, generation = loading.navigationGeneration
                        , rows = Dict.singleton "project:p" {key="project:p",kind="project",entityId="p",depth=0,parentKind="workspace_root",parentId=Nothing,zone=Nothing} } } }
                staleViewport = readable.cards.viewport
                readableCards = readable.cards
                stale = { readable | cards = { readableCards | viewport = { staleViewport | generation = staleViewport.generation - 1 } } }
                focus = readable.focus
                ancestorFocused = { readable | focus = { focus | focusedEntity = Just ("project","ancestor") } }
            in Expect.equal (True,False,True) (Navigation.returnReady readable,Navigation.returnReady stale,Navigation.returnReady ancestorFocused)
        , test "direct graph Back opens an unfocused full-hierarchy fallback instead of forcing project focus" <| \_ ->
            let
                direct = { base | url = { url | fragment = Just "tab=projects&task-graph=p" } }
                graph = Navigation.reconcile base (direct,Cmd.none) |> Tuple.first
                returned = Navigation.update TaskGraphBack graph |> Tuple.first
            in Expect.equal {focus=Nothing,query="",empty=True,project=Just "p"} {focus=returned.focus.focusedEntity,query=returned.search.query,empty=returned.search.filterShowEmptyProjects,project=Maybe.map .projectId returned.taskGraphNavigation.returning}
        , test "bounded origin eviction and a different authenticated audience fail closed" <| \_ ->
            let
                owner = {workspaceId="w",sessionEpoch=1,audienceId="a"}
                entry = {guard=owner,projectId="p",sourceUrl="source",graphUrl="graph",origin=()}
                state = List.range 1 65 |> List.foldl (\n -> History.remember (String.fromInt n) entry) History.init
            in Expect.equal {size=64,evicted=True,foreign=True,present=True} {size=Dict.size state.entries,evicted=History.lookup owner "1" state==Nothing,foreign=History.lookup {owner|audienceId="b"} "65" state==Nothing,present=History.lookup owner "65" state /= Nothing}
        ]


configure : Maybe (String,String) -> String -> Float -> Model -> Model
configure focus query top model =
    let
        currentFocus=model.focus
        search=model.search
        cards=model.cards
        viewport=cards.viewport
    in
    { model | focus={currentFocus|focusedEntity=focus},search={search|query=query,filterTaskStatuses=["done"]}, mainContentScrollY=top+10
        ,cards={cards|collapsedNodes=Dict.singleton "proj-p" (top>200),expandedCards=Dict.singleton "p" (top<200),viewport={viewport|top=top}} }


enter : Model -> (Model,String)
enter model =
    let
        opened=Navigation.update (OpenTaskGraph "p") model |> Tuple.first
        captured=Navigation.update (TaskGraphNavigationEvent (captureEvent opened)) opened |> Tuple.first
    in case captured.taskGraphNavigation.pending of
        Nothing -> (captured,"missing")
        Just pending ->
            let stamped=Url.fromString pending.entry.sourceUrl |> Maybe.andThen (\target -> Navigation.handleUrl target captured) |> Maybe.map Tuple.first |> Maybe.withDefault captured
                graph=Url.fromString pending.entry.graphUrl |> Maybe.andThen (\target -> Navigation.handleUrl target stamped) |> Maybe.map Tuple.first |> Maybe.withDefault stamped
            in (graph,pending.id)


source : String -> Model -> Model
source id model =
    Dict.get id model.taskGraphNavigation.history.entries |> Maybe.andThen (.sourceUrl >> Url.fromString)
        |> Maybe.andThen (\target -> Navigation.handleUrl target model) |> Maybe.map Tuple.first |> Maybe.withDefault model


captureEvent : Model -> Encode.Value
captureEvent model =
    Encode.object [ ("kind",Encode.string "captured"),("id",Encode.string (model.taskGraphNavigation.pending |> Maybe.map .id |> Maybe.withDefault "missing"))
        ,("scope",Encode.object [("workspace",Encode.string "w"),("epoch",Encode.int 1),("audience",Encode.string "reader")])
        ,("top",Encode.float model.mainContentScrollY),("anchor",Encode.float 24)]


base : Model
base =
    let
        initial=AppShell.initModel Nothing url (WorkspacePage "w") {apiUrl="",wsUrl="",sessionId="runtime",runtimeMode="test",authTokenStorageKey="token",authTokenPresent=False,loginUrl=Nothing,logoutUrl=Nothing} Nothing {tab=ProjectsTab,focus=Nothing,observationId=Nothing}
        session={authMode="test",effectiveActor=Nothing,principal={actorType="user",actorId="reader",actorLabel="Reader",authority="test",grantUserId=Nothing},globalPermissions={superadmin=False,createWorkspace=False},workspace=Just {workspaceId="w",role=Just "read",canRead=True,canEdit=False,canAdmin=False}}
        project={id="p",workspaceId="w",parentId=Nothing,name="Project",description=Nothing,status=Api.ProjActive,priority=8,createdAt="now",updatedAt="now"}
        ready={initial|auth={status=AuthReady,mode=Just "test"},selectedWorkspaceId=Just "w",sessionRequestEpoch=1,sessionContext=Just session,projects=Dict.singleton "p" project}
    in Navigation.reconcile ready (ready,Cmd.none) |> Tuple.first


url : Url.Url
url =
    {protocol=Url.Http,host="app.test",port_=Nothing,path="/workspace/w",query=Nothing,fragment=Just "tab=projects"}
