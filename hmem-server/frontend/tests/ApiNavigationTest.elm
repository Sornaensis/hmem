module ApiNavigationTest exposing (suite)

import Api
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "bounded navigation API transport"
        [ test "encodes exact branch, focus, dependency page, and summary batch requests" <|
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
