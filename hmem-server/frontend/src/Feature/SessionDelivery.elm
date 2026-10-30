module Feature.SessionDelivery exposing
    ( State, Target(..), Payload(..), Request, Resource, init, key, demand, removeDemand
    , invalidateSession, invalidateProgress, invalidateTask, invalidateLists, invalidateFacets, invalidateReadProof, invalidateWorkspace, invalidateAll, retireWorkspace, retireAll
    , admit, requestIsCurrent, settle, settleSelected, retry, retrySelected, select, RetryTicket, Selection, RetryState(..), transient, retryDelay, fetch, staleAt, consume
    )

{-| Bounded demand for canonical session HTTP resources. Logical retirement
never releases a physical admission; only its exact callback does that.
-}

import Api
import Dict exposing (Dict)
import Http
import Set exposing (Set)


type Target
    = WorkspaceSessions (Maybe Api.SessionCursor)
    | WorkspaceSessionsFiltered Api.SessionFilters (Maybe Api.SessionCursor)
    | ScopedSessions (Maybe String) (Maybe String) Api.SessionFilters (Maybe Api.SessionCursor)
    | SessionFacets String (Maybe String) (Maybe String) (Maybe String) (Maybe String)
    | TaskSessionsFiltered String Api.SessionFilters (Maybe Api.SessionCursor)
    | TaskSessions String (Maybe Api.SessionCursor)
    | SessionMetadata String
    | SessionTasks String (Maybe String)
    | SessionBytes String Int Bool
    | SessionWindow String Int Int Bool
    | SessionSave String Int Int
    | TaskEvidenceArchive String Int
    | SessionArchiveHttp String Int


type Payload
    = SessionsPage Api.SessionPage
    | Metadata Api.AgentSession
    | TasksPage Api.SessionTaskPage
    | Bytes Api.SessionRange
    | ArchiveDownloaded
    | FacetsPage Api.SessionFacetPage


type RetryState
    = Available
    | Waiting RetryTicket
    | Spent


type alias Selection =
    { workspace : String, session : String, lifetime : String }


type alias RetryTicket =
    { request : Request, selection : Selection }


retryDelay : Float
retryDelay = 500


type alias Resource =
    { workspace : String, target : Target, dirty : Int, settled : Int
    , data : Maybe Payload, ranges : List Api.SessionRange, error : Maybe String, proven : Bool, incarnation : Int, retry : RetryState }


type alias Request =
    { id : Int, key : String, workspace : String, target : Target, dirty : Int, proofRetirement : Int, incarnation : Int }


type alias State =
    { resources : Dict String Resource, physical : Dict Int Request, order : List String, implicitMetadata : Set String
    , selection : Maybe Selection, nextId : Int, version : Int, retirement : Int, proofRetirement : Int, listInvalidation : Int, foregroundStreak : Int, admissionError : Maybe String }


init : State
init =
    { resources = Dict.empty, physical = Dict.empty, order = [], implicitMetadata = Set.empty, selection = Nothing, nextId = 1, version = 0, retirement = 0, proofRetirement = 0, listInvalidation = 0, foregroundStreak = 0, admissionError = Nothing }


key : String -> Target -> String
key workspace target =
    let
        cursor value = value |> Maybe.map (\item -> item.createdAt ++ ":" ++ item.id) |> Maybe.withDefault "first"
        suffix = case target of
            WorkspaceSessions before -> "list:" ++ cursor before
            WorkspaceSessionsFiltered filters before -> "list:" ++ filters.state ++ ":" ++ filters.freshness ++ ":" ++ Maybe.withDefault "legacy" filters.activity ++ ":" ++ cursor before
            ScopedSessions project task filters before -> "scoped:" ++ Maybe.withDefault "all" project ++ ":" ++ Maybe.withDefault "all" task ++ ":" ++ filters.state ++ ":" ++ filters.freshness ++ ":" ++ Maybe.withDefault "legacy" filters.activity ++ ":" ++ cursor before
            SessionFacets kind project after selected activity -> "facets:" ++ kind ++ ":" ++ Maybe.withDefault "all" project ++ ":" ++ Maybe.withDefault "first" after ++ ":" ++ Maybe.withDefault "none" selected ++ ":" ++ Maybe.withDefault "legacy" activity
            TaskSessionsFiltered task filters before -> "task-filter:" ++ task ++ ":" ++ filters.state ++ ":" ++ filters.freshness ++ ":" ++ Maybe.withDefault "legacy" filters.activity ++ ":" ++ cursor before
            TaskEvidenceArchive task operation -> "archive:" ++ task ++ ":" ++ String.fromInt operation
            SessionArchiveHttp id sequence -> "sealed:" ++ id ++ ":" ++ String.fromInt sequence
            TaskSessions task before -> "task:" ++ task ++ ":" ++ cursor before
            SessionMetadata id -> "detail:" ++ id
            SessionTasks id after -> "links:" ++ id ++ ":" ++ Maybe.withDefault "first" after
            SessionBytes id _ _ -> "range:" ++ id
            SessionWindow id _ _ _ -> "window:" ++ id
            SessionSave id _ _ -> "save:" ++ id
    in
    "workspace:" ++ workspace ++ "|sessions:" ++ suffix


demand : String -> Target -> State -> State
demand workspace target state =
    case target of
        SessionBytes id offset _ ->
            if offset < 0 then { state | admissionError = Just "invalid_range" }
            else if Dict.size state.resources + (List.filter (\identity -> not (Dict.member identity state.resources)) [key workspace target,key workspace (SessionMetadata id)] |> List.length) > 16 then
                { state | admissionError = Just "demand_limit" }
            else
                let
                    metadataKey = key workspace (SessionMetadata id)
                    withMetadata = demandOne workspace (SessionMetadata id) state
                    owned = if Dict.member metadataKey state.resources then withMetadata else { withMetadata | implicitMetadata = Set.insert metadataKey withMetadata.implicitMetadata }
                in
                demandOne workspace target owned
        SessionMetadata _ ->
            let explicit = demandOne workspace target state in
            { explicit | implicitMetadata = Set.remove (key workspace target) explicit.implicitMetadata }
        SessionWindow id offset end _ ->
            if offset < 0 || end <= offset || end - offset > 2097152 then { state | admissionError = Just "invalid_window" }
            else demandPaired workspace target id state
        SessionSave id offset count ->
            if offset < 0 || count < 1 || count > 65536 then { state | admissionError = Just "invalid_save_chunk" }
            else demandPaired workspace target id state
        _ -> demandOne workspace target state


demandPaired : String -> Target -> String -> State -> State
demandPaired workspace target id state =
    if Dict.size state.resources + (List.filter (\identity -> not (Dict.member identity state.resources)) [key workspace target,key workspace (SessionMetadata id)] |> List.length) > 16 then
        { state | admissionError = Just "demand_limit" }
    else
        let
            metadataKey = key workspace (SessionMetadata id)
            withMetadata = demandOne workspace (SessionMetadata id) state
            owned = if Dict.member metadataKey state.resources then withMetadata else { withMetadata | implicitMetadata = Set.insert metadataKey withMetadata.implicitMetadata }
        in demandOne workspace target owned


pairedId : Target -> Maybe String
pairedId target = case target of
    SessionBytes id _ _ -> Just id
    SessionWindow id _ _ _ -> Just id
    SessionSave id _ _ -> Just id
    _ -> Nothing


demandOne : String -> Target -> State -> State
demandOne workspace target state =
    let
        identity = key workspace target
        version = state.version + 1
    in
    case Dict.get identity state.resources of
        Just resource ->
            if resource.target == target && (resource.error == Nothing || isSelected resource state) then state
            else { state | version = version, admissionError = Nothing
                , resources = Dict.insert identity { resource | target = target, dirty = version, error = Nothing, incarnation = version, retry = Available } state.resources }
        Nothing ->
            if Dict.size state.resources >= 16 then { state | admissionError = Just "demand_limit" }
            else { state | version = version, admissionError = Nothing, order = state.order ++ [identity]
                , resources = Dict.insert identity (Resource workspace target version 0 Nothing [] Nothing False version Available) state.resources }


-- Selection lifetimes are independent of progress invalidation versions.
isSelected : Resource -> State -> Bool
isSelected resource state =
    state.selection |> Maybe.map (\selection ->
        resource.workspace == selection.workspace && (case resource.target of
            SessionMetadata id -> id == selection.session
            SessionBytes id _ _ -> id == selection.session
            SessionWindow id _ _ _ -> id == selection.session
            _ -> False)) |> Maybe.withDefault False


select : Maybe Selection -> State -> State
select selection state =
    if selection == state.selection then state else
    let
        version = state.version + 1
        next = {state | selection=selection,version=version}
    in
    {next | resources=Dict.map (\_ resource ->
        if isSelected resource next then
            {resource | incarnation=version,retry=Available,error=Nothing}
        else if isSelected resource state then
            {resource | incarnation=version,retry=Spent}
        else resource) state.resources}


retrySelected : State -> State
retrySelected state =
    let version=state.version+1 in
    {state | version=version,resources=Dict.map (\_ resource ->
        if isSelected resource state then {resource | incarnation=version,retry=Available,error=Nothing,dirty=version} else resource) state.resources}


transient : Http.Error -> Bool
transient error =
    case error of
        Http.Timeout -> True
        Http.NetworkError -> True
        Http.BadStatus status -> List.member status [500,502,503,504]
        _ -> False


-- A timer only restores queued demand. Admission and callback-owned release
-- remain the scheduler's responsibility.
retry : RetryTicket -> State -> State
retry ticket state =
    let request=ticket.request in
    case Dict.get request.key state.resources of
        Just resource ->
            if state.selection == Just ticket.selection && isSelected resource state
                && resource.incarnation == request.incarnation && resource.target == request.target
                && state.proofRetirement == request.proofRetirement && resource.retry == Waiting ticket then
                {state | resources=Dict.insert request.key {resource | retry=Spent,error=Nothing} state.resources}
            else state
        Nothing -> state


removeDemand : String -> Target -> State -> State
removeDemand workspace target state =
    let
        remove identity source = { source | resources = Dict.remove identity source.resources, order = List.filter ((/=) identity) source.order, implicitMetadata = Set.remove identity source.implicitMetadata, admissionError = Nothing }
    in
    case target of
        SessionMetadata id ->
            if Dict.values state.resources |> List.any (\resource -> resource.workspace == workspace && pairedId resource.target == Just id) then
                { state | implicitMetadata = Set.insert (key workspace target) state.implicitMetadata }
            else remove (key workspace target) state
        _ ->
            let without = remove (key workspace target) state in
            case pairedId target of
                Just id ->
                    let metadataKey = key workspace (SessionMetadata id) in
                    if Set.member metadataKey state.implicitMetadata && not (Dict.values without.resources |> List.any (\resource -> resource.workspace == workspace && pairedId resource.target == Just id)) then remove metadataKey without else without
                Nothing -> without


invalidate : (Resource -> Bool) -> State -> State
invalidate matches state =
    let version = state.version + 1 in
    { state | version = version, listInvalidation = state.listInvalidation + 1, resources = Dict.map (\_ resource ->
        if matches resource then { resource | dirty = version, error = if isSelected resource state then resource.error else Nothing } else resource) state.resources }


invalidateWorkspace : String -> State -> State
invalidateWorkspace workspace =
    invalidateReadProof workspace >> (\state -> {state | retirement=state.retirement+1})


-- Membership changes invalidate canonical read proofs, not the independent
-- authorization/lifetime of an immutable evidence writer or task ZIP export.
invalidateReadProof : String -> State -> State
invalidateReadProof workspace =
    invalidate (\resource -> resource.workspace == workspace)
        >> (\state -> { state | proofRetirement = state.proofRetirement + 1, resources = Dict.map (\_ resource ->
            if resource.workspace == workspace then { resource | data = retainedMetadata resource.data, ranges = [], proven = False, incarnation = state.version, retry = Available, error = Nothing } else resource) state.resources })


invalidateAll : State -> State
invalidateAll =
    invalidate (always True) >> (\state -> { state | retirement = state.retirement + 1, proofRetirement = state.proofRetirement + 1, resources = Dict.map (\_ resource -> { resource | data = retainedMetadata resource.data, ranges = [], proven = False, incarnation = state.version, retry = Available, error = Nothing }) state.resources })


-- Same-authority refresh keeps the last admitted presentation in this existing
-- bounded cache. Resync retires its read proof; ordinary output invalidations
-- retain the admitted canonical prefix while a newer refresh is outstanding.
retainedMetadata : Maybe Payload -> Maybe Payload
retainedMetadata value =
    case value of
        Just (SessionsPage _) -> value
        Just (Metadata _) -> value
        Just (TasksPage _) -> value
        Just (FacetsPage _) -> value
        _ -> Nothing


invalidateSession : String -> String -> State -> State
invalidateSession workspace id =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        SessionMetadata actual -> actual == id
        SessionTasks _ _ -> False
        WorkspaceSessions _ -> True
        WorkspaceSessionsFiltered _ _ -> True
        ScopedSessions _ _ _ _ -> True
        SessionFacets _ _ _ _ _ -> False
        TaskSessionsFiltered _ _ _ -> True
        TaskSessions _ _ -> True
        SessionBytes _ _ _ -> False
        SessionWindow _ _ _ _ -> False
        SessionSave _ _ _ -> False
        TaskEvidenceArchive _ _ -> False
        SessionArchiveHttp _ _ -> False))


-- Progress does not change task/list membership. Reuse the selected session's
-- metadata read; otherwise refresh only pages that actually display it. Freshness
-- filters can gain a missing member on heartbeat. An existing fresh member is
-- stable under a newer heartbeat; quiet membership still needs a canonical read.
invalidateProgress : String -> String -> State -> State
invalidateProgress workspace id state =
    let
        version = state.version + 1
        hasMetadata = Dict.member (key workspace (SessionMetadata id)) state.resources
        matches resource =
            case resource.target of
                SessionMetadata actual -> actual == id
                _ ->
                    let
                        contains = case resource.data of
                            Just (SessionsPage page) -> List.any (.id >> (==) id) page.items
                            _ -> False
                        freshnessChanged filters = case filters.activity of
                            Nothing -> filters.freshness /= "" && (filters.freshness /= "fresh" || not contains)
                            Just activity ->
                                let selected=String.split "," activity
                                    live=List.member "live" selected
                                    quiet=List.member "quiet" selected
                                in (live && not quiet && not contains) || (quiet && not live)
                        freshness = case resource.target of
                            WorkspaceSessionsFiltered filters _ -> freshnessChanged filters
                            ScopedSessions _ _ filters _ -> freshnessChanged filters
                            TaskSessionsFiltered _ filters _ -> freshnessChanged filters
                            _ -> False
                    in freshness || (not hasMetadata && contains)
    in
    { state | version = version, resources = Dict.map (\_ resource ->
        if resource.workspace == workspace && matches resource then { resource | dirty = version, error = if isSelected resource state then resource.error else Nothing } else resource) state.resources }


invalidateTask : String -> String -> State -> State
invalidateTask workspace task =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        TaskSessionsFiltered actual _ _ -> actual == task
        TaskSessions actual _ -> actual == task
        _ -> False))


invalidateLists : String -> State -> State
invalidateLists workspace =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        WorkspaceSessions _ -> True
        WorkspaceSessionsFiltered _ _ -> True
        ScopedSessions _ _ _ _ -> True
        SessionFacets _ _ _ _ _ -> False
        TaskSessionsFiltered _ _ _ -> True
        _ -> False))


invalidateFacets : String -> State -> State
invalidateFacets workspace =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        SessionFacets _ _ _ _ _ -> True
        _ -> False))


retireWorkspace : String -> State -> State
retireWorkspace workspace state =
    let kept = Dict.filter (\_ resource -> resource.workspace /= workspace) state.resources in
    { state | retirement = state.retirement + 1, proofRetirement = state.proofRetirement + 1, resources = kept, order = List.filter (\id -> Dict.member id kept) state.order, implicitMetadata = Set.filter (\id -> Dict.member id kept) state.implicitMetadata }


retireAll : State -> State
retireAll state =
    { state | retirement = state.retirement + 1, proofRetirement = state.proofRetirement + 1, resources = Dict.empty, order = [], implicitMetadata = Set.empty, admissionError = Nothing }


admit : String -> State -> ( State, List Request )
admit workspace state =
    let
        occupied identity = Dict.values state.physical |> List.any (\request -> request.key == identity)
        metadataReady resource = case pairedId resource.target of
            Just id -> Dict.get (key workspace (SessionMetadata id)) state.resources
                |> Maybe.map (\detail -> detail.proven && detail.error == Nothing && detail.data /= Nothing) |> Maybe.withDefault False
            Nothing -> True
        ready = state.order |> List.filterMap (\identity -> Dict.get identity state.resources |> Maybe.map (Tuple.pair identity)) |> List.filter (\( identity, resource ) ->
            resource.workspace == workspace && resource.dirty /= resource.settled && resource.error == Nothing && (case resource.retry of
                Waiting _ -> False
                _ -> True) && not (occupied identity) && metadataReady resource)
        foreground (_, resource) = case resource.target of
            SessionMetadata _ -> True
            SessionBytes _ _ _ -> True
            SessionWindow _ _ _ _ -> True
            _ -> False
        choose remaining available current requests =
            if remaining <= 0 then (current, requests) else
            let
                urgent = List.filter foreground available
                background = List.filter (not << foreground) available
                candidate = if current.foregroundStreak < 3 || List.isEmpty background then
                    case List.head urgent of
                        Just item -> Just item
                        Nothing -> List.head background
                    else List.head background
            in
            case candidate of
                Nothing -> (current, requests)
                Just item ->
                    let
                        (next, admitted) = add item (current, requests)
                        paced = {next | foregroundStreak = if foreground item then min 3 (current.foregroundStreak + 1) else 0}
                    in choose (remaining - 1) (List.filter (Tuple.first >> (/=) (Tuple.first item)) available) paced admitted
        add ( identity, resource ) ( current, requests ) =
            let request = Request current.nextId identity workspace resource.target resource.dirty current.proofRetirement resource.incarnation in
            ( { current | nextId = current.nextId + 1, physical = Dict.insert request.id request current.physical, order = List.filter ((/=) identity) current.order ++ [identity] }, requests ++ [request] )
    in
    choose (max 0 (2 - Dict.size state.physical)) ready state []


requestIsCurrent : Request -> State -> Bool
requestIsCurrent request state =
    request.proofRetirement == state.proofRetirement
        && (Dict.get request.key state.resources |> Maybe.map (\resource ->
            resource.incarnation == request.incarnation && resource.target == request.target) |> Maybe.withDefault False)


settle : Int -> Bool -> Result Http.Error Payload -> State -> State
settle id authorized result state =
    settleSelected id authorized result state |> Tuple.first


settleSelected : Int -> Bool -> Result Http.Error Payload -> State -> ( State, Maybe RetryTicket )
settleSelected id authorized result state =
    let
        finished = settleResult id authorized result state
        ticket = Dict.get id state.physical |> Maybe.andThen (\request -> Dict.get request.key finished.resources |> Maybe.andThen (\resource -> case resource.retry of
            Waiting value -> if value.request.id == id then Just value else Nothing
            _ -> Nothing))
    in (finished,ticket)


settleResult : Int -> Bool -> Result Http.Error Payload -> State -> State
settleResult id authorized result state =
    case Dict.get id state.physical of
        Nothing -> state
        Just request ->
            let released = { state | physical = Dict.remove id state.physical } in
            case Dict.get request.key state.resources of
                Nothing -> released
                Just resource ->
                    if not authorized || request.proofRetirement /= state.proofRetirement || resource.target /= request.target || resource.incarnation /= request.incarnation then released
                    else if resource.dirty /= request.dirty && (case result of
                        Err _ -> not (isSelected resource state)
                        Ok (Metadata _) -> False
                        Ok (SessionsPage _) -> False
                        Ok (TasksPage _) -> False
                        Ok (FacetsPage _) -> False
                        _ -> True) then released
                    else case result of
                        Err error ->
                            let
                                retryState = case (resource.retry,state.selection) of
                                    (Available,Just selection) -> if isSelected resource state && transient error then Waiting {request=request,selection=selection} else Spent
                                    _ -> Spent
                            in
                            { released | resources = Dict.insert request.key { resource | data = Nothing, ranges = [], error = Just "refresh_failed", proven = False, retry = retryState } released.resources }
                        Ok payload ->
                            if not (valid request payload) then
                                { released | resources = Dict.insert request.key { resource | data = Nothing, ranges = [], error = Just "invalid_response", proven = False } released.resources }
                            else if not (monotonic resource.data payload && monotonicCached request payload state) then released
                            else
                                apply request resource payload released


-- One physical metadata read per key plus these monotonic guards prevents an
-- older canonical prefix from reopening a sealed session or rewinding bytes.
monotonic : Maybe Payload -> Payload -> Bool
monotonic previous incoming =
    case (previous,incoming) of
        (Just (Metadata old),Metadata next) ->
            next.committedBytes >= old.committedBytes && next.revision >= old.revision
                && next.runtimeRevision >= old.runtimeRevision
                && heartbeatMonotonic old.lastSeenAt next.lastSeenAt
                && occurrenceMonotonic old next
                && (old.state /= Api.SessionTerminated || next.state == Api.SessionTerminated)
        (Just (SessionsPage old),SessionsPage next) ->
            List.all (\session -> old.items |> List.filter (.id >> (==) session.id) |> List.head |> Maybe.map (\prior -> monotonic (Just (Metadata prior)) (Metadata session)) |> Maybe.withDefault True) next.items
        _ -> True


-- A first detail read can race a newer list response, and vice versa. Both
-- transcript and runtime revisions remain floors across these bounded caches.
monotonicCached : Request -> Payload -> State -> Bool
monotonicCached request payload state =
    case payload of
        Metadata session ->
            Dict.values state.resources |> List.filter (\entry -> entry.workspace == request.workspace && entry.proven)
                |> List.all (\entry -> case entry.data of
                    Just (SessionsPage page) -> page.items |> List.filter (.id >> (==) session.id) |> List.all (\old -> monotonic (Just (Metadata old)) payload)
                    _ -> True)
        SessionsPage page ->
            List.all (\session -> Dict.get (key request.workspace (SessionMetadata session.id)) state.resources
                |> Maybe.map (\entry -> not entry.proven || monotonic entry.data (Metadata session)) |> Maybe.withDefault True) page.items
        _ -> True


-- Canonical server timestamps use UTC Z with optional fractional seconds. Pad
-- the fraction before comparison so whole-second and fractional forms agree.
heartbeatMonotonic : String -> String -> Bool
heartbeatMonotonic old next =
    let
        stamp value =
            if not (String.endsWith "Z" value) then Nothing else
            case String.split "." (String.dropRight 1 value) of
                [ seconds ] -> Just (seconds, String.repeat 12 "0")
                [ seconds, fraction ] -> Just (seconds, String.padRight 12 '0' fraction)
                _ -> Nothing
    in
    case (stamp old,stamp next) of
        (Just before,Just after) -> before <= after
        _ -> True


-- Same-cursor derivation completes independently of transcript revision. A
-- delayed pending response cannot undo a completed projection in another cache.
-- A genuinely newer cursor may be pending until its canonical derivation ends.
occurrenceMonotonic : Api.AgentSession -> Api.AgentSession -> Bool
occurrenceMonotonic old next =
    case (old.lastActivityAt,next.lastActivityAt) of
        (Just before,Just after) -> heartbeatMonotonic before after
        (Just _,Nothing) -> next.committedBytes > old.committedBytes
        _ -> True


valid : Request -> Payload -> Bool
valid request payload =
    let sessionMatches id session = session.id == id && session.workspaceId == request.workspace in
    case ( request.target, payload ) of
        ( WorkspaceSessions _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( WorkspaceSessionsFiltered _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( ScopedSessions _ _ _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( SessionFacets _ _ _ _ _, FacetsPage page ) -> List.length page.items <= 50
        ( TaskSessionsFiltered _ _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( TaskEvidenceArchive _ _, ArchiveDownloaded ) -> True
        ( SessionArchiveHttp _ _, ArchiveDownloaded ) -> True
        ( TaskSessions _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( SessionMetadata id, Metadata session ) -> sessionMatches id session
        ( SessionTasks _ _, TasksPage page ) -> List.length page.items <= 50
        ( SessionBytes id offset sealed, Bytes range ) ->
            validRange request.workspace id offset 65536 sealed range
        ( SessionWindow id offset end sealed, Bytes range ) -> validRange request.workspace id offset (min 65536 (end-offset)) sealed range
        ( SessionSave id offset count, Bytes range ) -> validRange request.workspace id offset count True range
        _ -> False


validRange : String -> String -> Int -> Int -> Bool -> Api.SessionRange -> Bool
validRange workspace id offset count sealed range =
    range.session.id == id && range.session.workspaceId == workspace && range.offset == offset && range.nextOffset >= offset
        && range.nextOffset <= offset + count && range.nextOffset <= range.session.committedBytes
        && (not range.hasMore || range.nextOffset > offset)
        && range.hasMore == (range.nextOffset < range.session.committedBytes)
        && (range.session.state == Api.SessionTerminated) == sealed
        && base64ByteLength range.dataBase64 == Just (range.nextOffset - offset)


consume : String -> Target -> State -> State
consume workspace target state =
    { state | resources = Dict.update (key workspace target) (Maybe.map (\resource -> { resource | data = Nothing, ranges = [] })) state.resources }


-- Validate standard padded base64, including zero pad bits, without decoding
-- transcript bytes as Unicode or allocating an unbounded second copy.
base64ByteLength : String -> Maybe Int
base64ByteLength encoded =
    let
        size = String.length encoded
        padding = if String.endsWith "==" encoded then 2 else if String.endsWith "=" encoded then 1 else 0
        body = String.dropRight padding encoded
        alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        index ch = String.indexes (String.fromChar ch) alphabet |> List.head
        finalIndex = String.right 1 body |> String.toList |> List.head |> Maybe.andThen index
        padBits = case padding of
            2 -> finalIndex |> Maybe.map (\value -> modBy 16 value == 0) |> Maybe.withDefault False
            1 -> finalIndex |> Maybe.map (\value -> modBy 4 value == 0) |> Maybe.withDefault False
            _ -> True
    in
    if size <= 87384 && modBy 4 size == 0 && String.all (\ch -> index ch /= Nothing) body && padBits then
        Just (size // 4 * 3 - padding)
    else Nothing


apply : Request -> Resource -> Payload -> State -> State
apply request resource payload state =
    let
        cached = { resource | data = Just payload, settled = request.dirty, error = Nothing, proven = True, retry = Available }
        updated = { state | resources = Dict.insert request.key cached state.resources }
    in
    case payload of
        Metadata session ->
            let
                changed old = old.state /= session.state || old.title /= session.title || old.parentSessionId /= session.parentSessionId
                previous = case resource.data of
                    Just (Metadata old) -> [old]
                    _ -> []
                pageEntries = Dict.values state.resources |> List.filter (\entry -> entry.workspace == request.workspace && entry.proven)
                    |> List.concatMap (\entry -> case entry.data of
                        Just (SessionsPage page) -> List.filter (.id >> (==) session.id) page.items
                        _ -> [])
                membershipChanged = List.any changed (previous ++ pageEntries)
                patchPage entry =
                    if entry.workspace /= request.workspace || not entry.proven then entry else
                    case entry.data of
                        Just (SessionsPage page) ->
                            {entry | data = Just (SessionsPage {page | items = List.map (\old -> if old.id == session.id && monotonic (Just (Metadata old)) (Metadata session) then session else old) page.items})}
                        _ -> entry
                synchronized = {updated | resources = Dict.map (\_ entry -> patchPage entry) updated.resources}
                refreshed = if membershipChanged then invalidateLists request.workspace synchronized else synchronized
                rangeKey = key request.workspace (SessionBytes session.id 0 False)
                updateRange old =
                    case old.target of
                        SessionBytes id offset sealed ->
                            if sealed /= (session.state == Api.SessionTerminated) || (session.committedBytes > offset && not (Dict.values refreshed.physical |> List.any (\pending -> pending.key == rangeKey))) then
                                { old | target = SessionBytes id offset (session.state == Api.SessionTerminated), dirty = refreshed.version + 1
                                    , error = if sealed /= (session.state == Api.SessionTerminated) then Nothing else old.error
                                    , retry = if sealed /= (session.state == Api.SessionTerminated) then Available else old.retry
                                    , incarnation = if sealed /= (session.state == Api.SessionTerminated) then refreshed.version + 1 else old.incarnation }
                            else if session.committedBytes == offset then { old | settled = old.dirty }
                            else old
                        _ -> old
            in
            { refreshed | version = refreshed.version + 1, resources = Dict.update rangeKey (Maybe.map updateRange) refreshed.resources }
        Bytes range ->
            if pairedId request.target /= Nothing && (case request.target of
                SessionBytes _ _ _ -> False
                _ -> True) then updated
            else
            let
                nextVersion = updated.version + 1
                metadataAhead = Dict.get (key request.workspace (SessionMetadata range.session.id)) updated.resources
                    |> Maybe.andThen .data |> Maybe.map (\value -> case value of
                        Metadata latest -> latest.committedBytes > range.nextOffset
                        _ -> False) |> Maybe.withDefault False
                next = { cached | ranges = List.take 4 (range :: resource.ranges)
                    , target = SessionBytes range.session.id range.nextOffset (range.session.state == Api.SessionTerminated)
                    , dirty = if range.hasMore || metadataAhead then nextVersion else request.dirty }
            in
            { updated | version = nextVersion, resources = Dict.insert request.key next updated.resources }
        _ -> updated


fetch : String -> Request -> (Result Http.Error Payload -> msg) -> Cmd msg
fetch apiUrl request toMsg =
    case request.target of
        WorkspaceSessions cursor -> Api.fetchAgentSessions apiUrl request.workspace Nothing cursor 50 (Result.map SessionsPage >> toMsg)
        WorkspaceSessionsFiltered filters cursor -> Api.fetchFilteredAgentSessions apiUrl request.workspace Nothing filters cursor 50 (Result.map SessionsPage >> toMsg)
        ScopedSessions project task filters cursor -> Api.fetchScopedAgentSessions apiUrl request.workspace project task filters cursor (Result.map SessionsPage >> toMsg)
        SessionFacets kind project after selected activity -> Api.fetchSessionFacets apiUrl request.workspace kind project after selected activity (Result.map FacetsPage >> toMsg)
        TaskSessionsFiltered task filters cursor -> Api.fetchFilteredAgentSessions apiUrl request.workspace (Just task) filters cursor 50 (Result.map SessionsPage >> toMsg)
        TaskEvidenceArchive _ _ -> Cmd.none
        SessionArchiveHttp _ _ -> Cmd.none
        TaskSessions task cursor -> Api.fetchAgentSessions apiUrl request.workspace (Just task) cursor 50 (Result.map SessionsPage >> toMsg)
        SessionMetadata id -> Api.fetchAgentSession apiUrl request.workspace id (Result.map Metadata >> toMsg)
        SessionTasks id cursor -> Api.fetchSessionTasks apiUrl request.workspace id cursor 50 (Result.map TasksPage >> toMsg)
        SessionBytes id offset sealed -> Api.fetchSessionRange apiUrl request.workspace id sealed offset 65536 (Result.map Bytes >> toMsg)
        SessionWindow id offset end sealed -> Api.fetchSessionRange apiUrl request.workspace id sealed offset (min 65536 (end-offset)) (Result.map Bytes >> toMsg)
        SessionSave id offset count -> Api.fetchSessionRange apiUrl request.workspace id True offset count (Result.map Bytes >> toMsg)


staleAt : Float -> Float -> Bool
staleAt now lastHeartbeat =
    now - lastHeartbeat >= 30000
