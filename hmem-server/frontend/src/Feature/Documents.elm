module Feature.Documents exposing
    ( availability, beginRead, count, init, invalidate, pending, reconcile, refreshTask, retireAssociation, scope, update
    , viewDialog, viewPanel
    )

import Api
import AuditNavigation
import Dict
import Helpers
import Html exposing (Html, button, div, h3, h4, p, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, id, title, type_)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Http
import Icon
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Ports
import Set
import StartupAdmission
import Types exposing (..)
import Url


init : DocumentsModel
init =
    { scope = "", transferStamp = ( "", "" ), demand = Set.empty, reads = Dict.empty, visibleThumbnails = Set.empty, dialog = Nothing
    , styledAttempt = 0, nextOperation = 0, operation = Nothing, notice = ""
    }


scope : Model -> String
scope model =
    case ( model.selectedWorkspaceId, model.sessionContext ) of
        ( Just workspace, Just session ) ->
            if model.auth.status == AuthReady && model.page == WorkspacePage workspace && model.activeTab == ProjectsTab && Permissions.canReadCurrentWorkspace model then
                Encode.encode 0 (Encode.list Encode.string
                    [ workspace, String.fromInt model.sessionRequestEpoch, session.principal.actorType
                    , session.principal.actorId, session.principal.authority
                    , Maybe.withDefault "" session.principal.grantUserId
                    , if Permissions.canEditCurrentWorkspace model then "edit" else "read"
                    ])
            else
                ""

        _ ->
            ""


emptyRead : DocumentRead
emptyRead =
    { request = Nothing, dirty = True, failed = False, value = Nothing, cursor = Nothing }


setDocuments : (DocumentsModel -> DocumentsModel) -> Model -> Model
setDocuments change model =
    { model | documents = change model.documents }


sync : Model -> Model
sync model =
    let
        old = model.documents
        owner = scope model
        state =
            if old.scope == owner then
                old
            else
                { init | scope = owner, transferStamp = old.transferStamp, nextOperation = old.nextOperation }

        demand =
            if owner == "" then
                Set.empty
            else
                let
                    ordinary = model.dataLoading.visibleDetailDemand |> Maybe.map Tuple.second |> Maybe.withDefault Set.empty
                    pins = model.dataLoading.viewportDetailPins |> Set.filter (Tuple.first >> (==) "task") |> Set.map Tuple.second
                    viewport = model.cards.viewport
                    mountedFocus =
                        case model.focus.focusedEntity of
                            Just ( "task", task ) ->
                                -- Direct focus pins its row even outside ordinary navigation membership.
                                if viewport.workspaceId == model.selectedWorkspaceId
                                    && viewport.sessionEpoch == model.sessionRequestEpoch
                                    && viewport.generation == model.dataLoading.navigationGeneration
                                    && Dict.member ("task:" ++ task) viewport.rows
                                    && AuditNavigation.destinationStatus model == Nothing then
                                    Set.singleton task
                                else
                                    Set.empty

                            _ ->
                                Set.empty
                in
                Set.union (Set.union ordinary pins) mountedFocus
                    |> Set.filter (\task -> Helpers.isExpanded model task && Dict.member task model.tasks)

        dialogTask =
            case state.dialog of
                Just (UploadSourceDialog task) -> Just task
                Just (PreviewDocumentDialog item _ _) -> Just item.taskId
                Nothing -> Nothing

        dialog =
            if dialogTask |> Maybe.map (\task -> Set.member task demand) |> Maybe.withDefault False then
                state.dialog
            else
                Nothing

        thumbnails =
            Set.filter (Tuple.first >> (\task -> Set.member task demand)) state.visibleThumbnails

        wanted =
            Set.toList demand
                |> List.concatMap (\task ->
                    ( task, "counts" ) ::
                        (case Dict.get task model.cards.taskSections of
                            Just "sources" -> [ ( task, "source" ) ]
                            Just "evidence" -> [ ( task, "evidence" ) ]
                            _ -> []
                        ))
                |> (++) (Set.toList thumbnails)
                |> (\keys -> case dialog of
                    Just (PreviewDocumentDialog item _ _) -> ( item.taskId, previewKey item ) :: keys
                    _ -> keys
                )

        reads =
            List.foldl
                (\key entries -> Dict.insert key (Dict.get key state.reads |> Maybe.withDefault emptyRead) entries)
                Dict.empty wanted
    in
    { model | documents = { state | demand = demand, reads = reads, dialog = dialog, visibleThumbnails = thumbnails } }


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, command ) =
    let
        next = sync model
        after = next.documents
        stamp = ( after.scope, dialogIdentity after.dialog )
        transferOwner =
            Encode.object
                [ ( "scope", Encode.string after.scope )
                , ( "dialog", Encode.string (dialogIdentity after.dialog) )
                ]
    in
    ( { next | documents = { after | transferStamp = stamp } }
    , if after.transferStamp /= stamp then
        Cmd.batch [ command, Ports.documentTransferOwner transferOwner ]
      else
        command
    )


pending : Model -> List ( String, String )
pending model =
    Dict.toList model.documents.reads
        |> List.filter (\( _, entry ) -> entry.dirty && not entry.failed && entry.request == Nothing)
        |> List.map Tuple.first
        |> List.sortBy (\( _, kind ) -> if String.startsWith "preview:" kind then 0 else if kind == "counts" then 2 else 1)


beginRead : ( String, String ) -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
beginRead (( task, kind ) as key) ( model, commands ) =
    case ( model.selectedWorkspaceId, Dict.get key model.documents.reads ) of
        ( Just workspace, Just entry ) ->
            let
                loading = model.dataLoading
                capacity = Set.size loading.cardDetailAdmissions + Dict.size model.auditDiff.admissions
                    + (Dict.values model.move.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length)
            in
            if model.move.dialog /= Nothing || capacity >= 6 then
                ( model, commands )
            else
                let
                    request = loading.nextCardDetailRequestId
                    base = Api.documentBase model.flags.apiUrl workspace task
                    complete convert result =
                        TaskDocuments (DocumentReadFinished model.documents.scope key request (Result.map convert result))
                    command =
                        case kind of
                            "counts" -> Api.fetchDocumentCounts base (complete DocumentCountsValue)
                            "source" -> Api.fetchDocumentPage base kind entry.cursor (complete DocumentPageValue)
                            "evidence" -> Api.fetchDocumentPage base kind entry.cursor (complete DocumentPageValue)
                            _ ->
                                Api.fetchDocumentPreview (base ++ "/" ++ Url.percentEncode (String.dropLeft 8 kind))
                                    (complete DocumentPreviewValue)
                    next =
                        setDocuments (\state -> { state | reads = Dict.insert key { entry | request = Just request, dirty = False } state.reads }) model
                in
                ( { next | dataLoading = { loading | cardDetailAdmissions = Set.insert request loading.cardDetailAdmissions, nextCardDetailRequestId = request + 1 } }
                , command :: commands
                )

        _ ->
            ( model, commands )


invalidate : Bool -> Set.Set String -> Model -> Model
invalidate countsOnly tasks =
    invalidateReads countsOnly tasks False


refreshTask : String -> Model -> Model
refreshTask task =
    invalidateReads False (Set.singleton task) True


invalidateReads : Bool -> Set.Set String -> Bool -> Model -> Model
invalidateReads countsOnly tasks preserveStyled model =
    setDocuments
        (\state ->
            let
                styled =
                    case state.dialog of
                        Just (PreviewDocumentDialog item _ _) ->
                            if item.mediaType == "text/html" && Set.member item.taskId tasks then
                                Just ( item.taskId, previewKey item )
                            else
                                Nothing
                        _ ->
                            Nothing
            in
            { state
                | reads = Dict.map
                    (\(( task, kind ) as key) entry ->
                        if Set.member task tasks && (not countsOnly || kind == "counts") then
                            { entry | dirty = True, failed = False, cursor = Nothing
                            , value = if preserveStyled && styled == Just key then entry.value else Nothing
                            }
                        else
                            entry
                    ) state.reads
                -- A resync cannot use document-scoped metadata to reauthorize the old association.
                , styledAttempt = if styled /= Nothing && not countsOnly && not preserveStyled then state.styledAttempt + 1 else state.styledAttempt
            })
        model


retireAssociation : String -> String -> Model -> Model
retireAssociation task association =
    setDocuments (\state ->
        case state.dialog of
            Just (PreviewDocumentDialog item _ _) ->
                if item.taskId == task && item.id == association then { state | dialog = Nothing } else state
            _ ->
                state
    )


count : String -> Model -> String -> Maybe Int
count kind model task =
    currentEntry model ( task, "counts" )
        |> Maybe.andThen .value
        |> Maybe.andThen (\value -> case value of
            DocumentCountsValue counts ->
                Just (case kind of
                    "source" -> counts.sourceCount
                    "evidence" -> counts.documentEvidenceCount
                    "sessions" -> counts.agentSessionEvidenceCount
                    _ -> counts.evidenceTotal
                )
            _ -> Nothing
        )


availability : String -> Model -> String -> Maybe Bool
availability workspace model task =
    if model.selectedWorkspaceId == Just workspace then
        count "total" model task |> Maybe.map ((/=) 0)
    else
        Nothing


currentEntry : Model -> ( String, String ) -> Maybe DocumentRead
currentEntry model key =
    if model.documents.scope /= "" && model.documents.scope == scope model then
        Dict.get key model.documents.reads
    else
        Nothing


previewKey : Api.DocumentItem -> String
previewKey item =
    "preview:" ++ item.documentId


dialogIdentity : Maybe DocumentDialog -> String
dialogIdentity dialog =
    case dialog of
        Just (UploadSourceDialog task) -> "upload:" ++ task
        Just (PreviewDocumentDialog item _ _) -> "preview:" ++ item.id
        Nothing -> ""


update : DocumentMsg -> Model -> ( Model, Cmd Msg )
update msg source =
    let
        model = sync source
        owner = model.documents.scope
        modify change = ( setDocuments change model, Cmd.none )
        updateRead task kind change state =
            { state | reads = Dict.update ( task, kind ) (Maybe.map change) state.reads }
        retryRead task kind =
            updateRead task kind (\entry -> { entry | dirty = True, failed = False, value = Nothing })
        target task =
            owner /= "" && Set.member task model.documents.demand
        write item removedAssociation send =
            case model.selectedWorkspaceId of
                Just workspace ->
                    if target item.taskId && Permissions.canEditCurrentWorkspace model && model.documents.operation == Nothing then
                        let
                            token = model.documents.nextOperation
                            url = Api.documentBase model.flags.apiUrl workspace item.taskId ++ "/" ++ Url.percentEncode item.documentId
                        in
                        ( setDocuments (\state -> { state | operation = Just token, nextOperation = token + 1, notice = "" }) model
                        , send url (DocumentWriteFinished owner token item.taskId removedAssociation >> TaskDocuments)
                        )
                    else
                        ( model, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
    in
    case msg of
        DocumentReadFinished requestScope (( task, kind ) as key) request result ->
            let
                loading = model.dataLoading
                valid entry =
                    case result of
                        Ok (DocumentPageValue page) ->
                            if List.all (\item -> item.taskId == task && item.role == kind) page.items && (page.next == Nothing || page.next /= entry.cursor) then
                                Just (DocumentPageValue page)
                            else
                                Nothing
                        other ->
                            Result.toMaybe other
                settle entry =
                    if entry.request /= Just request then
                        entry
                    else if entry.dirty then
                        { entry | request = Nothing }
                    else
                        let accepted = valid entry in
                        { entry | request = Nothing, failed = accepted == Nothing, value = accepted }
                released = { model | dataLoading = { loading
                    | cardDetailAdmissions = Set.remove request loading.cardDetailAdmissions
                    , startupAdmission = StartupAdmission.complete ("detail|" ++ String.fromInt request) loading.startupAdmission
                    } }
            in
            ( if owner == requestScope then
                setDocuments (\state -> { state | reads = Dict.update key (Maybe.map settle) state.reads }) released
              else
                released
            , Cmd.none
            )

        RetryDocumentRead task kind ->
            modify (retryRead task kind)

        DocumentPageChanged task kind cursor ->
            modify (updateRead task kind (\entry -> { entry | dirty = True, failed = False, cursor = cursor, value = Nothing }))

        OpenUploadSource task ->
            if target task && Permissions.canEditCurrentWorkspace model then
                modify (\state -> { state | dialog = Just (UploadSourceDialog task), notice = "" })
            else
                ( model, Cmd.none )

        OpenDocumentPreview item ->
            if target item.taskId then
                modify (\state -> { state | dialog = Just (PreviewDocumentDialog item (if item.mediaType == "application/pdf" then 1 else 0) False), notice = "" })
            else
                ( model, Cmd.none )

        CloseDocumentDialog ->
            modify (\state -> { state | dialog = Nothing })

        DocumentPreviewPage page ->
            modify (\state -> { state | dialog = case state.dialog of
                Just (PreviewDocumentDialog item _ zoom) -> Just (PreviewDocumentDialog item page zoom)
                other -> other
            })

        ToggleDocumentZoom ->
            modify (\state -> { state | dialog = case state.dialog of
                Just (PreviewDocumentDialog item page zoom) -> Just (PreviewDocumentDialog item page (not zoom))
                other -> other
            })

        RemoveDocumentSource item ->
            if item.role == "source" then write item (Just item.id) Api.removeDocumentSource else ( model, Cmd.none )

        RetryDocumentPreview item ->
            write item Nothing Api.retryDocumentPreview

        DocumentWriteFinished requestScope token task removedAssociation result ->
            if requestScope /= owner || model.documents.operation /= Just token then
                ( model, Cmd.none )
            else
                ( setDocuments (\state -> { state | operation = Nothing, notice = case result of
                    Ok () -> ""
                    Err _ -> "Document change could not finish. Please retry."
                  })
                    (case result of
                        Ok () ->
                            removedAssociation
                                |> Maybe.map (\association -> retireAssociation task association model)
                                |> Maybe.withDefault model
                                |> refreshTask task
                        Err _ -> model
                    )
                , Cmd.none
                )

        DocumentTransferEvent value ->
            let
                field name = Decode.decodeValue (Decode.field name Decode.string) value |> Result.withDefault ""
                message = field "message"
                task = field "task"
                kind = field "kind"
                key = ( task, "preview:" ++ field "document" )
            in
            if owner == "" || field "scope" /= owner || not (target task) then
                ( model, Cmd.none )
            else if kind == "thumbnail-visible" || kind == "thumbnail-hidden" then
                modify (\state -> { state | visibleThumbnails =
                    if kind == "thumbnail-hidden" then Set.remove key state.visibleThumbnails
                    else if Set.size state.visibleThumbnails >= 12 then state.visibleThumbnails
                    else Set.insert key state.visibleThumbnails })
            else if field "dialog" /= dialogIdentity model.documents.dialog then
                ( model, Cmd.none )
            else if kind == "retry-styled" then
                case model.documents.dialog of
                    Just (PreviewDocumentDialog item _ _) ->
                        if item.taskId == task && item.mediaType == "text/html" then
                            -- A fast same-generation response can arrive before Elm paints the loading state.
                            modify (\state -> retryRead task (previewKey item) { state | styledAttempt = state.styledAttempt + 1 })
                        else
                            ( model, Cmd.none )
                    _ ->
                        ( model, Cmd.none )
            else if kind == "uploaded" then
                ( invalidate False (Set.singleton task) model |> setDocuments (\state -> { state | dialog = Nothing, notice = "Source uploaded." }), Cmd.none )
            else
                modify (\state -> { state | notice = message })

viewPanel : String -> Model -> String -> Html Msg
viewPanel role model task =
    let
        entry = currentEntry model ( task, role )
        page =
            entry |> Maybe.andThen .value |> Maybe.andThen (\value -> case value of
                DocumentPageValue result -> Just result
                _ -> Nothing)
        label = if role == "source" then "Sources" else "Document evidence"
        control = TaskDocuments
    in
    Html.section [ class "task-documents", attribute "aria-label" label ]
        [ div [ class "task-documents-heading" ]
            [ h4 [ class "card-section-heading" ] [ text (label ++ countSuffix (count role model task)) ]
            , if role == "source" && Permissions.canEditCurrentWorkspace model then
                button [ id ("document-upload-" ++ task), attribute "data-dialog-owner" model.documents.scope, class "btn btn-sm", type_ "button", onClick (control (OpenUploadSource task)) ] [ text "Upload source" ]
              else
                text ""
            ]
        , if model.documents.notice /= "" && model.documents.dialog == Nothing then
            p [ class "card-meta", attribute "role" "status" ] [ text model.documents.notice ]
          else
            text ""
        , case page of
            Just result ->
                if List.isEmpty result.items then
                    p [ class "card-meta" ] [ text (if role == "source" then "No Sources." else "No Document evidence.") ]
                else
                    Keyed.node "div" [ class "task-document-list" ] (List.map (\item -> ( item.id, viewRow model item )) result.items)
            Nothing ->
                if entry |> Maybe.map .failed |> Maybe.withDefault False then
                    p [ class "card-meta", attribute "role" "status" ]
                        [ text "Documents could not be loaded. "
                        , button [ class "btn btn-sm", type_ "button", onClick (control (RetryDocumentRead task role)) ] [ text "Retry" ] ]
                else
                    p [ class "card-meta", attribute "role" "status" ] [ text "Loading documents…" ]
        , if entry |> Maybe.map .cursor |> Maybe.withDefault Nothing |> (/=) Nothing then
            button [ class "btn btn-sm", type_ "button", onClick (control (DocumentPageChanged task role Nothing)) ] [ text "Newest documents" ]
          else
            text ""
        , page |> Maybe.andThen .next
            |> Maybe.map (\cursor -> button [ class "btn btn-sm", type_ "button", onClick (control (DocumentPageChanged task role (Just cursor))) ] [ text "Older documents" ])
            |> Maybe.withDefault (text "")
        ]


countSuffix : Maybe Int -> String
countSuffix value =
    value |> Maybe.map (String.fromInt >> (++) " · ") |> Maybe.withDefault ""


byteLabel : Int -> String
byteLabel bytes =
    String.fromInt bytes ++ " bytes"


documentUrl : Model -> Api.DocumentItem -> String
documentUrl model item =
    Api.documentBase model.flags.apiUrl (Maybe.withDefault "" model.selectedWorkspaceId) item.taskId ++ "/" ++ Url.percentEncode item.documentId


viewRow : Model -> Api.DocumentItem -> Html Msg
viewRow model item =
    let
        videoPreview = item.mediaType == "video/mp4"
        thumbnailPreview = videoPreview || List.member item.mediaType [ "image/png", "image/jpeg", "image/webp" ]
        previewable = thumbnailPreview || List.member item.mediaType [ "application/pdf", "text/html" ]
        previewButton buttonClass contents =
            button
                [ id ("document-preview-" ++ item.id), type_ "button", class buttonClass
                , attribute "aria-label" ("Preview " ++ item.filename), attribute "data-dialog-owner" model.documents.scope
                , onClick (TaskDocuments (OpenDocumentPreview item)) ]
                contents

        thumbnail =
            currentEntry model ( item.taskId, previewKey item )
                |> Maybe.andThen .value
                |> Maybe.andThen (\value -> case value of
                    DocumentPreviewValue preview ->
                        Maybe.map2
                            (\generation output -> rasterAttributes (documentUrl model item) generation output)
                            preview.generation
                            (List.filter (\output -> output.variant == "thumbnail" && output.page == 0 && inertRaster output) preview.outputs |> List.head)
                    _ -> Nothing)
    in
    div [ class "task-document-row" ]
        [ if thumbnailPreview then
            previewButton "document-thumbnail"
                [ if item.previewState == "ready" then
                    Html.img
                        ([ attribute "alt" "", attribute "data-document-thumbnail" item.documentId
                         , attribute "data-document-task" item.taskId, attribute "data-document-scope" model.documents.scope ]
                         ++ Maybe.withDefault [] thumbnail
                        ) []
                  else
                    text ""
                , span [ class "document-thumbnail-mark" ] [ Icon.view "zoom-in" ]
                ]
          else
            span [ class "task-document-icon" ]
                [ Icon.view (if item.mediaType == "text/html" then "file-code" else "file-text") ]
        , div [ class "task-document-label" ]
            [ Html.strong [] [ text (if String.isEmpty item.title then item.filename else item.title) ]
            , if item.title /= item.filename && not (String.isEmpty item.title) then span [] [ text item.filename ] else text ""
            , p [ class "card-meta" ]
                [ text (item.mediaType ++ " · " ++ byteLabel item.rawBytes ++ " · " ++ item.uploader ++ " · ")
                , Html.time [ attribute "datetime" item.createdAt ] [ text item.createdAt ]
                ]
            , p [ class "card-meta" ]
                [ text (if videoPreview && item.previewState == "ready" then "Video thumbnail · first frame" else previewState item.previewState) ]
            , if item.role == "source" && Permissions.canEditCurrentWorkspace model then
                button
                    [ type_ "button", class "task-document-remove", disabled (model.documents.operation /= Nothing)
                    , onClick (TaskDocuments (RemoveDocumentSource item))
                    , attribute "aria-label" ("Remove from Sources: " ++ item.filename)
                    ]
                    [ text "Remove from Sources" ]
              else
                text ""
            ]
        , div [ class "task-document-actions" ]
            [ if previewable && not thumbnailPreview then
                previewButton "btn btn-sm" [ text "Preview" ]
              else
                text ""
            , download model item
            ]
        ]


download : Model -> Api.DocumentItem -> Html Msg
download model item =
    button
        [ type_ "button", class "btn btn-sm session-download"
        , attribute "data-document-download" (documentUrl model item ++ "/original")
        , attribute "data-document-filename" item.filename
        , attribute "data-document-scope" model.documents.scope
        , attribute "data-document-task" item.taskId
        ]
        [ text "↓ Download" ]


previewState : String -> String
previewState state =
    case state of
        "queued" -> "Preview queued"
        "processing" -> "Preparing preview…"
        "ready" -> "Preview ready"
        "failed" -> "Preview failed"
        _ -> "Preview unavailable — export PNG or PDF"


inertRaster : Api.DocumentOutput -> Bool
inertRaster output =
    List.member output.mediaType [ "image/png", "image/jpeg", "image/webp" ]
        && output.rawBytes > 0 && output.rawBytes <= 104857600


rasterAttributes : String -> String -> Api.DocumentOutput -> List (Html.Attribute Msg)
rasterAttributes base generation output =
    [ attribute "data-document-raster" (base ++ "/previews/" ++ Url.percentEncode generation ++ "/" ++ output.variant ++ "/" ++ String.fromInt output.page)
    , attribute "data-document-raster-type" output.mediaType
    , attribute "data-document-raster-bytes" (String.fromInt output.rawBytes)
    ]


viewDialog : Model -> Html Msg
viewDialog model =
    case ( model.selectedWorkspaceId, model.documents.dialog ) of
        ( Just workspace, Just dialog ) ->
            let
                token = dialogIdentity (Just dialog)
                close = button [ type_ "button", class "btn btn-sm", attribute "autofocus" "", attribute "data-document-preview-close" "", onClick (TaskDocuments CloseDocumentDialog) ] [ text "Close" ]
                heading label = h3 [ id "task-document-dialog-title" ] [ text label ]
                ( origin, body, isPreview ) =
                    case dialog of
                        UploadSourceDialog task ->
                            ( "document-upload-" ++ task
                            , [ div [ class "task-document-dialog-head" ] [ heading "Upload source" ]
                              , Html.form
                                    [ attribute "data-document-upload" (Api.documentBase model.flags.apiUrl workspace task)
                                    , attribute "data-document-task" task
                                    , attribute "data-document-scope" model.documents.scope
                                    ]
                                    [ div [ class "task-document-upload-fields" ]
                                        [ Html.label [ attribute "for" "document-original-file" ] [ text "File" ]
                                        , Html.input
                                            [ id "document-original-file", type_ "file", attribute "name" "original"
                                            , attribute "required" "", attribute "accept" ".png,.jpg,.jpeg,.webp,.mp4,.pdf,.html,.htm,.svg,.txt,.md,.markdown,.docx,.pptx,.xlsx,.fig,.sketch"
                                            ] []
                                        , p [ class "card-meta" ] [ text "Up to 25 MiB." ]
                                        , Html.label [ attribute "for" "document-original-title" ] [ text "Title (optional)" ]
                                        , Html.input [ id "document-original-title", type_ "text", attribute "name" "title", attribute "maxlength" "255" ] []
                                        ]
                                    , div [ class "task-document-actions task-document-dialog-footer" ]
                                        [ close, button [ type_ "submit", class "btn btn-sm" ] [ text "Upload source" ] ]
                                    ]
                              ], False )
                        PreviewDocumentDialog item page zoom ->
                            ( "document-preview-" ++ item.id, viewPreview (heading item.filename) close model item page zoom, True )
            in
            Html.node "dialog"
                ([ id "task-document-dialog", classList [ ( "task-document-dialog", True ), ( "task-document-preview-dialog", isPreview ) ]
                , attribute "aria-labelledby" "task-document-dialog-title"
                , attribute "data-dialog-token" token
                , attribute "data-dialog-origin" origin
                , attribute "data-dialog-owner" model.documents.scope
                , attribute "data-document-preview-task" (case dialog of
                    UploadSourceDialog task -> task
                    PreviewDocumentDialog item _ _ -> item.taskId)
                , Html.Events.preventDefaultOn "cancel" (Decode.succeed ( TaskDocuments CloseDocumentDialog, True ))
                 ] ++ (if isPreview then [ attribute "closedby" "any" ] else []))
                (body ++
                 [ p [ class "card-meta task-document-dialog-notice", attribute "role" "status", attribute "data-document-transfer-status" "" ] [ text model.documents.notice ] ])
        _ ->
            text ""


viewPreview : Html Msg -> Html Msg -> Model -> Api.DocumentItem -> Int -> Bool -> List (Html Msg)
viewPreview heading close model item page zoom =
    let
        entry = currentEntry model ( item.taskId, previewKey item )
        preview = entry |> Maybe.andThen .value |> Maybe.andThen (\value -> case value of
            DocumentPreviewValue result -> Just result
            _ -> Nothing)
        failed = entry |> Maybe.map .failed |> Maybe.withDefault False
        url = documentUrl model item
        retryRead = button [ type_ "button", class "btn btn-sm", onClick (TaskDocuments (RetryDocumentRead item.taskId (previewKey item))) ] [ text "Retry" ]
        readError =
            div [ class "document-preview-status" ]
                [ p [ class "card-meta", attribute "role" "status" ] [ text "Preview information could not be loaded." ]
                , retryRead
                ]
        styledPreview state =
            let
                ready = state.state == "ready" && List.any (\output -> output.variant == "styled_html_preview" && output.page == 0 && output.mediaType == "text/html") state.outputs
            in
            case ( ready, state.generation ) of
                ( True, Just generation ) ->
                    Keyed.node "div" [ class "document-styled-stage" ]
                        [ ( generation ++ ":" ++ String.fromInt model.documents.styledAttempt
                          , div
                                [ class "document-styled-transfer"
                                , attribute "data-document-styled" (url ++ "/download-grants")
                                , attribute "data-document-generation" generation, attribute "data-document-association" item.id
                                , attribute "data-document-role" item.role, attribute "data-document-scope" model.documents.scope
                                , attribute "data-document-frame-title" ("Preview of " ++ item.filename)
                                ]
                                [ div [ class "document-styled-mount" ] []
                                , div [ class "document-preview-status" ]
                                    [ p [ class "card-meta", attribute "role" "status", attribute "data-document-frame-status" "" ] [ text "Loading preview…" ]
                                    , button [ type_ "button", class "btn btn-sm", attribute "data-document-frame-retry" "" ] [ text "Retry" ]
                                    ]
                                , span [ attribute "tabindex" "0", attribute "data-dialog-return" "" ] []
                                ]
                          )
                        ]
                _ ->
                    div [ class "document-styled-stage" ]
                        [ if state.state == "ready" then readError else p [ class "card-meta", attribute "role" "status" ] [ text (previewState state.state) ] ]
        frame state =
            case state.generation of
                Just generation ->
                    let
                        outputs = state.outputs |> List.filter (\output -> output.variant == "page")
                        raster = List.filter (\output -> output.page == page && inertRaster output) outputs |> List.head
                    in
                    case raster of
                          Just output ->
                            div [ class ("document-raster-viewport" ++ (if zoom then " document-raster-zoom" else "")) ]
                              [ Html.img
                                ([ attribute "alt" ("Preview of " ++ item.filename)
                                 , attribute "data-document-scope" model.documents.scope
                                 ] ++ rasterAttributes url generation output) []
                            , div [ class "task-document-actions" ]
                                [ if List.length outputs > 1 then
                                    button [ type_ "button", class "btn btn-sm", disabled (page <= 1), onClick (TaskDocuments (DocumentPreviewPage (page - 1))) ] [ text "Previous page" ]
                                  else text ""
                                , if List.length outputs > 1 then
                                    span [] [ text ("Page " ++ String.fromInt page ++ " of " ++ String.fromInt (List.length outputs)) ]
                                  else text ""
                                , if List.length outputs > 1 then
                                    button [ type_ "button", class "btn btn-sm", disabled (page >= List.length outputs), onClick (TaskDocuments (DocumentPreviewPage (page + 1))) ] [ text "Next page" ]
                                  else text ""
                                , button [ type_ "button", class "btn btn-sm", onClick (TaskDocuments ToggleDocumentZoom) ] [ text (if zoom then "Fit" else "Zoom") ]
                                ]
                              ]
                          Nothing ->
                            p [ class "card-meta" ] [ text "Preview is unavailable." ]
                Nothing ->
                    p [ class "card-meta" ] [ text (previewState state.state) ]
    in
    [ div [ class "task-document-dialog-head" ]
        [ div [ class "task-document-dialog-label" ]
            [ heading
            , p [ class "card-meta" ] [ text (item.mediaType ++ " · " ++ byteLabel item.rawBytes) ]
            ]
        , download model item
        , close
        ]
    , div [ class "task-document-preview-body" ]
        [ if item.mediaType == "video/mp4" && (preview |> Maybe.map (.state >> (==) "ready") |> Maybe.withDefault False) then
            p [ class "card-meta" ] [ text "Video preview · first frame shown" ]
          else if preview |> Maybe.map .animatedFirstFrame |> Maybe.withDefault False then
            p [ class "card-meta" ] [ text "Animated original · first frame shown" ]
          else
            text ""
        , if item.mediaType == "text/html" then
            if failed then
                div [ class "document-styled-stage" ] [ readError ]
            else
                preview |> Maybe.map styledPreview |> Maybe.withDefault
                    (div [ class "document-styled-stage" ] [ p [ class "card-meta", attribute "role" "status" ] [ text "Loading preview…" ] ])
          else if failed then
            p [ class "card-meta" ]
                [ text "Preview information could not be loaded. "
                , retryRead
                ]
          else
            preview |> Maybe.map frame |> Maybe.withDefault (p [ class "card-meta" ] [ text "Loading preview…" ])
        , if Permissions.canEditCurrentWorkspace model && (preview |> Maybe.map .retryable |> Maybe.withDefault False) then
            button [ type_ "button", class "btn btn-sm", disabled (model.documents.operation /= Nothing), onClick (TaskDocuments (RetryDocumentPreview item)) ] [ text "Retry preview" ]
          else
            text ""
        ]
    ]


