+++
schema = "adrai/connection/v1"
connection = "C01M4G8R5ZMAZZN1H5W5HZEXGNA"
relation = "applies_to"
added = ["api.md", "hmem-core/src/HMem/DB/Overview.hs", "hmem-core/src/HMem/DB/Pool.hs", "hmem-core/src/HMem/Types.hs", "hmem-server/migrations/V034__subtask_parent_auto_blocking.sql", "hmem-server/src/HMem/Server/API.hs"]
applies_to = ["api.md", "hmem-core/src/HMem/DB/Overview.hs", "hmem-core/src/HMem/DB/Pool.hs", "hmem-core/src/HMem/DB/Project.hs", "hmem-core/src/HMem/DB/Task.hs", "hmem-core/src/HMem/Types.hs", "hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql", "hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql", "hmem-server/migrations/V029__cascade_archive_and_cancel.sql", "hmem-server/migrations/V034__subtask_parent_auto_blocking.sql", "hmem-server/src/HMem/Server/API.hs"]
change = "expand"
parent_connections = ["C01M3YDMRKHKGHWQBKZDTQ452ST"]
removed = []
subject_adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
+++

Apply revised subtask-blocking lifecycle policy to its forward migration, readiness and API authority.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6Ijk4Y2U5YjgzMDU1OTk3YWMyZGYwMThiZmQ4NTI5NjFhYjYxMDc3YTgiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU00RzhSNVpNQVpaTjFINVc1SFpFWEdOQSIsIm9wIjoiTzAxTTRHOFI1Wk1BWlpOMUg1VzVIWkVYR05BIiwicCI6WyJDMDFNM1lETVJLSEtHSFdRQktaRFRRNDUyU1QiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6RlQxVzRDRHhTeVNRQWxRRHVJYnRTeFVMbVpldnBiVGhuLTdJNkJvNWlpZyIsInQiOjE3OTE1NDc0MTQ1MTYsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
