+++
schema = "adrai/connection/v1"
connection = "C01M4NFYWJDC1C9TSFDG617KNZ5"
relation = "applies_to"
added = ["hmem-core/test/HMem/DB/ChangeStreamSpec.hs", "hmem-core/test/HMem/DB/OverviewSpec.hs", "hmem-core/test/HMem/DB/ProjectSpec.hs", "hmem-core/test/HMem/DB/TaskSpec.hs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V035__project_pause_lifecycle.sql"]
applies_to = ["api.md", "hmem-core/src/HMem/DB/Overview.hs", "hmem-core/src/HMem/DB/Pool.hs", "hmem-core/src/HMem/DB/Project.hs", "hmem-core/src/HMem/DB/Task.hs", "hmem-core/src/HMem/Types.hs", "hmem-core/test/HMem/DB/ChangeStreamSpec.hs", "hmem-core/test/HMem/DB/OverviewSpec.hs", "hmem-core/test/HMem/DB/ProjectSpec.hs", "hmem-core/test/HMem/DB/TaskSpec.hs", "hmem-server/hmem-server.cabal", "hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql", "hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql", "hmem-server/migrations/V029__cascade_archive_and_cancel.sql", "hmem-server/migrations/V034__subtask_parent_auto_blocking.sql", "hmem-server/migrations/V035__project_pause_lifecycle.sql", "hmem-server/src/HMem/Server/API.hs"]
change = "expand"
parent_connections = ["C01M4G8R5ZMAZZN1H5W5HZEXGNA"]
removed = []
subject_adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
+++

Apply project pause lifecycle authority to its forward migration, packaging and core acceptance tests

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6ImM3ODlkMDQ0NDQxMzcwOTMwMWM1ODIzZWRkNDJkYjRlMzVmOTVjODUiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU00TkZZV0pEQzFDOVRTRkRHNjE3S05aNSIsIm9wIjoiTzAxTTRORllXSkRDMUM5VFNGREc2MTdLTlo1IiwicCI6WyJDMDFNNEc4UjVaTUFaWk4xSDVXNUhaRVhHTkEiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6ZU9yb2F2ZGZvX3NGa3haUWg5QUdwcFlTY3pNdzFoYWM5ZUoxWHZtOHJBWSIsInQiOjE3OTE3MjI3NDY0NDUsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
