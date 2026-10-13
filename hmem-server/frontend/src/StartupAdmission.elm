module StartupAdmission exposing (Owner, Phase(..), State, acknowledgeClosure, acknowledgeNormal, admit, admitOrdinary, arm, canOrdinary, complete, headerOnly, init, openInitialWave, reconcile, recordRecovery, remaining, retire)

import Set exposing (Set)


type alias Owner =
    { workspaceId : String, sessionEpoch : Int, audience : String }


type Phase
    = Initial
    | ClosureArmed String
    | AwaitNormalPaint String
    | HeaderOnly
    | Normal


{-| Startup debt belongs to authority, not a root generation or paint nonce.
Completions release physical occupancy without refunding transmitted credits.
-}
type alias State =
    { owner : Maybe Owner
    , authorized : Bool
    , spent : Int
    , initialWaveOpened : Bool
    , rootStarted : Bool
    , phase : Phase
    , physical : Set String
    }


init : State
init =
    { owner = Nothing, authorized = False, spent = 0, initialWaveOpened = False, rootStarted = False, phase = Initial, physical = Set.empty }


retire : State -> State
retire state =
    { state | authorized = False, phase = Initial }


reconcile : Maybe Owner -> State -> State
reconcile current state =
    case current of
        Nothing ->
            retire state

        Just owner ->
            if state.owner == Just owner then
                { state | authorized = True }

            else
                { init | owner = Just owner, authorized = True, physical = state.physical }


remaining : State -> Int
remaining state =
    max 0 (6 - state.spent)


admit : String -> State -> Maybe State
admit key state =
    if not state.authorized || Set.member key state.physical then
        Nothing

    else
        case state.phase of
            Normal ->
                Just { state | physical = Set.insert key state.physical }

            HeaderOnly ->
                Just { state | physical = Set.insert key state.physical }

            Initial ->
                if remaining state > 0 then
                    Just { state | spent = state.spent + 1, physical = Set.insert key state.physical }

                else
                    Nothing

            _ ->
                Nothing


complete : String -> State -> State
complete key state =
    { state | physical = Set.remove key state.physical }


{-| Root retries and refreshes are necessary foreground recovery. They cannot
be parked behind a paint they must produce, but their debt reduces the initial
ordinary allowance. This is not a universal ceiling on recovery traffic.
-}
recordRecovery : String -> State -> State
recordRecovery key state =
    if not state.authorized || Set.member key state.physical then
        state
    else
        { state | physical = Set.insert key state.physical
            , spent = if not state.rootStarted || state.phase == Normal || state.phase == HeaderOnly then state.spent else state.spent + 1
            , rootStarted = True
            , phase = if state.phase == Normal || state.phase == HeaderOnly then state.phase else Initial
        }


admitOrdinary : String -> State -> Maybe State
admitOrdinary key state =
    if canOrdinary state then
        admit key state

    else
        Nothing


canOrdinary : State -> Bool
canOrdinary state =
    state.authorized && (state.phase == Normal || (state.phase == Initial && state.initialWaveOpened && remaining state > 0))


openInitialWave : State -> State
openInitialWave state =
    if state.authorized && state.phase == Initial && not state.initialWaveOpened then
        { state | initialWaveOpened = True }

    else
        state


arm : String -> Bool -> State -> State
arm fingerprint eligible state =
    let
        waveFinished =
            state.initialWaveOpened || remaining state == 0
    in
    if state.phase == Normal || state.phase == HeaderOnly then
        state

    else if not eligible || not state.authorized || not (Set.isEmpty state.physical) then
        { state | phase = Initial }

    else
        case state.phase of
            AwaitNormalPaint previous ->
                if previous == fingerprint then state else { state | phase = Initial }

            ClosureArmed previous ->
                if previous == fingerprint then state else { state | phase = ClosureArmed fingerprint }

            _ ->
                if waveFinished then { state | phase = ClosureArmed fingerprint } else state


acknowledgeClosure : String -> State -> State
acknowledgeClosure fingerprint state =
    if state.authorized && state.phase == ClosureArmed fingerprint && Set.isEmpty state.physical then
        { state | phase = AwaitNormalPaint fingerprint }

    else
        state


acknowledgeNormal : String -> State -> State
acknowledgeNormal fingerprint state =
    if state.authorized && state.phase == AwaitNormalPaint fingerprint && Set.isEmpty state.physical then
        { state | phase = Normal }

    else
        state


headerOnly : State -> State
headerOnly state =
    if state.authorized && Set.isEmpty state.physical then
        { state | phase = HeaderOnly, initialWaveOpened = True }

    else
        state
