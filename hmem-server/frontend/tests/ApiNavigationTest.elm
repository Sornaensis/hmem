module ApiNavigationTest exposing (suite)

import Api
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "bounded navigation API transport"
        [ test "batch IDs stay bounded while complete long filters and asymmetric cursors stay in the body" <| \_ ->
            let
                query = "description needle " ++ String.repeat 1024 "é"
                item = { requestId = "17", parentKind = "project", parentId = "owner", projectOffset = 0, taskOffset = 450, includeProjects = False, includeTasks = True }
                body = Api.encodeNavigationBatchRequest (Encode.object [ ( "query", Encode.string query ), ( "task_statuses", Encode.list Encode.string [] ) ]) [ item ]
            in
            Expect.equal { id = Ok "17", query = Ok query, offset = Ok 450, included = Ok False }
                { id = Decode.decodeValue (Decode.at [ "requests" ] (Decode.index 0 (Decode.field "request_id" Decode.string))) body
                , query = Decode.decodeValue (Decode.at [ "filters", "query" ] Decode.string) body
                , offset = Decode.decodeValue (Decode.at [ "requests" ] (Decode.index 0 (Decode.field "task_offset" Decode.int))) body
                , included = Decode.decodeValue (Decode.at [ "requests" ] (Decode.index 0 (Decode.field "include_projects" Decode.bool))) body }
        , test "batch validates complete ordered identities and rejects mixed success/error or foreign owner payloads" <| \_ ->
            let
                page = "{\"workspace_id\":\"workspace\",\"parent\":{\"kind\":\"project\",\"parent_id\":\"owner\"},\"projects\":{\"items\":[],\"has_more\":false},\"tasks\":{\"items\":[],\"has_more\":false}}"
                value suffix = "{\"workspace_id\":\"workspace\",\"outcomes\":[{\"request_id\":\"17\",\"parent\":{\"kind\":\"project\",\"parent_id\":\"owner\"},\"status\":200,\"response\":" ++ page ++ suffix ++ "}]}"
                item = { requestId = "17", parentKind = "project", parentId = "owner", projectOffset = 0, taskOffset = 0, includeProjects = True, includeTasks = True }
                decoded = Decode.decodeString Api.navigationBatchDecoder (value "") |> Result.toMaybe
                accepted = decoded |> Maybe.map (Api.validateNavigationBatchResponse "workspace" [ item ] >> Result.toMaybe >> (/=) Nothing) |> Maybe.withDefault False
                missing = decoded |> Maybe.map (\response -> Api.validateNavigationBatchResponse "workspace" [ item, { item | requestId = "18" } ] response |> Result.toMaybe |> (==) Nothing) |> Maybe.withDefault False
            in
            Expect.equal (List.repeat 4 True)
                [ accepted, missing
                , Decode.decodeString Api.navigationBatchDecoder (value ",\"error\":\"mixed\"") |> Result.toMaybe |> (==) Nothing
                , Decode.decodeString Api.navigationBatchDecoder (String.replace "\"parent_id\":\"owner\"},\"projects\"" "\"parent_id\":\"foreign\"},\"projects\"" (value "")) |> Result.toMaybe |> (==) Nothing ]
        , test "encodes independent project and task demand without changing offsets or limits" <|
            \_ ->
                Expect.equal
                    [ "&include_projects=false&include_tasks=true", "&include_projects=true&include_tasks=false", "&include_projects=false&include_tasks=false" ]
                    (List.map (\( projects, tasks ) ->
                        Api.navigationBranchUrlWithDemand "" "workspace" "project" (Just "parent") 0 450 projects tasks ""
                            |> String.dropLeft (String.length (Api.navigationBranchUrl "" "workspace" "project" (Just "parent") 0 450 "")))
                        [ ( False, True ), ( True, False ), ( False, False ) ])
        , test "due-free task decoding retains completion and hierarchy fields" <|
            \_ ->
                Decode.decodeString Api.taskDecoder """{"id":"subtask","workspace_id":"workspace","project_id":"project","parent_id":"parent","title":"Subtask","description":null,"status":"done","priority":7,"completed_at":"2026-10-14T12:00:00Z","dependency_count":2,"created_at":"created","updated_at":"updated"}"""
                    |> Result.map (\task -> ( task.parentId, task.completedAt, task.dependencyCount ))
                    |> Expect.equal (Ok ( Just "parent", Just "2026-10-14T12:00:00Z", 2 ))
        , test "encodes exact branch, focus, dependency page, and summary batch requests" <|
            \_ ->
                Expect.equal
                    { branch = "https://api.example/api/v1/workspaces/workspace-1/navigation?parent_kind=project&parent_id=parent-1&project_limit=50&project_offset=50&task_limit=50&task_offset=0&query=needle"
                    , focus = "https://api.example/api/v1/workspaces/workspace-1/navigation/focus/task/task-1?ancestor_offset=64"
                    , dependency = "https://api.example/api/v1/tasks/task-1/dependencies?limit=50&offset=50"
                    , summaries = Just "{\"project_ids\":[\"project-1\"],\"task_ids\":[\"task-1\"]}"
                    }
                    { branch = Api.navigationBranchUrl "https://api.example" "workspace-1" "project" (Just "parent-1") 50 0 "&query=needle"
                    , focus = Api.navigationFocusUrl "https://api.example" "workspace-1" "task" "task-1" 64
                    , dependency = Api.taskDependencyPageUrl "https://api.example" "task-1" 50
                    , summaries = Api.navigationSummariesBody [ "project-1" ] [ "task-1" ] |> Maybe.map (Encode.encode 0)
                    }
        , test "rejects oversized and duplicate summary batch request bodies" <|
            \_ ->
                Expect.equal ( Nothing, Nothing )
                    ( Api.navigationSummariesBody (List.range 1 101 |> List.map String.fromInt) []
                    , Api.navigationSummariesBody [ "duplicate" ] [ "duplicate" ]
                    )
        , test "rejects response pages and batches beyond their public caps" <|
            \_ ->
                Expect.equal True
                    (List.all (\value -> value)
                        [ Decode.decodeString Api.navigationBranchDecoder oversizedBranch |> isErr
                        , Decode.decodeString Api.navigationFocusDecoder oversizedFocus |> isErr
                        , Decode.decodeString Api.navigationSummariesDecoder oversizedSummaryBatch |> isErr
                        , Decode.decodeString Api.taskDependencyPageDecoder oversizedDependencyPage |> isErr
                        ]
                    )
        , test "rejects a focus continuation whose truncation marker and offset disagree" <|
            \_ ->
                Expect.equal True
                    (isErr <|
                        Decode.decodeString Api.navigationFocusDecoder
                            ("{\"workspace_id\":\"workspace-1\",\"target\":" ++ navigationProject "target" ++ ",\"ancestors\":[],\"ancestors_truncated\":true,\"next_ancestor_offset\":null}")
                    )
        ]


isErr : Result Decode.Error a -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


project : String -> String
project id =
    "{\"id\":\"" ++ id ++ "\",\"workspace_id\":\"workspace-1\",\"parent_id\":null,\"name\":\"" ++ id ++ "\",\"status\":\"active\",\"priority\":1,\"created_at\":\"2026-01-01T00:00:00Z\",\"updated_at\":\"2026-01-01T00:00:00Z\",\"direct_project_count\":0,\"direct_task_count\":0,\"has_children\":false,\"readiness_rollup\":{}}"


navigationProject : String -> String
navigationProject id =
    "{\"entity_type\":\"project\",\"summary\":" ++ project id ++ "}"


oversizedBranch : String
oversizedBranch =
    "{\"workspace_id\":\"workspace-1\",\"projects\":{\"items\":[" ++ projectItems 101 ++ "],\"has_more\":true},\"tasks\":{\"items\":[],\"has_more\":false}}"


oversizedFocus : String
oversizedFocus =
    "{\"workspace_id\":\"workspace-1\",\"target\":" ++ navigationProject "target" ++ ",\"ancestors\":[" ++ navigationProjects 65 ++ "],\"ancestors_truncated\":true,\"next_ancestor_offset\":64}"


oversizedSummaryBatch : String
oversizedSummaryBatch =
    "{\"projects\":[],\"tasks\":[],\"missing_project_ids\":[" ++ ids 101 ++ "],\"missing_task_ids\":[]}"


oversizedDependencyPage : String
oversizedDependencyPage =
    "{\"items\":[" ++ dependencyItems 101 ++ "],\"has_more\":true}"


projectItems : Int -> String
projectItems count =
    List.range 1 count
        |> List.map (\index -> project ("project-" ++ String.fromInt index))
        |> String.join ","


navigationProjects : Int -> String
navigationProjects count =
    List.range 1 count
        |> List.map (\index -> navigationProject ("ancestor-" ++ String.fromInt index))
        |> String.join ","


ids : Int -> String
ids count =
    List.range 1 count
        |> List.map (\index -> "\"missing-" ++ String.fromInt index ++ "\"")
        |> String.join ","


dependencyItems : Int -> String
dependencyItems count =
    List.range 1 count
        |> List.map (\index -> "{\"id\":\"task-" ++ String.fromInt index ++ "\",\"name\":\"Task " ++ String.fromInt index ++ "\"}")
        |> String.join ","
