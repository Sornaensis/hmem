+++
schema = "adrai/connection/v1"
connection = "C01M4NJSA6AN5GT3KJ70Z2E00P5"
relation = "applies_to"
added = ["docker.md", "hmem-core/test/HMem/DB/TestHarnessSpec.hs", "hmem-server/migrations/V036__normalize_project_pause.sql"]
applies_to = ["api.md", "docker.md", "hmem-core/src/HMem/DB/Overview.hs", "hmem-core/src/HMem/DB/Pool.hs", "hmem-core/src/HMem/DB/Project.hs", "hmem-core/src/HMem/DB/Task.hs", "hmem-core/src/HMem/Types.hs", "hmem-core/test/HMem/DB/ChangeStreamSpec.hs", "hmem-core/test/HMem/DB/OverviewSpec.hs", "hmem-core/test/HMem/DB/ProjectSpec.hs", "hmem-core/test/HMem/DB/TaskSpec.hs", "hmem-core/test/HMem/DB/TestHarnessSpec.hs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql", "hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql", "hmem-server/migrations/V029__cascade_archive_and_cancel.sql", "hmem-server/migrations/V034__subtask_parent_auto_blocking.sql", "hmem-server/migrations/V035__project_pause_lifecycle.sql", "hmem-server/migrations/V036__normalize_project_pause.sql", "hmem-server/src/HMem/Server/API.hs"]
change = "expand"
parent_connections = ["C01M4NFYWJDC1C9TSFDG617KNZ5"]
removed = []
subject_adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
+++

Cover atomic pause normalization, deferred provenance, upgrade fixtures and operational guidance

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjM1YmU3MzBkMzQ4NTI3ZWFhYzE5MjNkNzc1OGY4MDMzNjM4Y2Y1MDciLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU00TkpTQTZBTjVHVDNLSjcwWjJFMDBQNSIsIm9wIjoiTzAxTTROSlNBNkFONUdUM0tKNzBaMkUwMFA1IiwicCI6WyJDMDFNNE5GWVdKREMxQzlUU0ZERzYxN0tOWjUiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6ZHVCOHVINDRuNktkdzJmZDRmOGRNMHg2Q2FzcE5JT2RzRHF6QTJwMjlwRSIsInQiOjE3OTE3MjU3MDk1MTQsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
