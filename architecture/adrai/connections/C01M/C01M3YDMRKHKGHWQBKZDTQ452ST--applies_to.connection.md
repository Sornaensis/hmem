+++
schema = "adrai/connection/v1"
connection = "C01M3YDMRKHKGHWQBKZDTQ452ST"
relation = "applies_to"
added = ["hmem-server/migrations/V029__cascade_archive_and_cancel.sql"]
applies_to = ["hmem-core/src/HMem/DB/Project.hs", "hmem-core/src/HMem/DB/Task.hs", "hmem-server/migrations/V017__flat_subtask_lifecycle_rules.sql", "hmem-server/migrations/V027__bounded_task_dependency_cycle_check.sql", "hmem-server/migrations/V029__cascade_archive_and_cancel.sql"]
change = "expand"
parent_connections = ["C01M39R0FV2DFF19Z1FAHVA657R"]
removed = []
subject_adr = "A01M39R0FHX6Y5VC60CAHQDAX0D"
+++

Apply the amended lifecycle authority to its forward archive/cancel cascade migration.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjBkNmYzNjg2YjRmNjgyMWRlYjkxZWU3Nzg4ZmZlNDkwMmFmOGE1NTEiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU0zWURNUktIS0dIV1FCS1pEVFE0NTJTVCIsIm9wIjoiTzAxTTNZRE1SS0hLR0hXUUJLWkRUUTQ1MlNUIiwicCI6WyJDMDFNMzlSMEZWMkRGRjE5WjFGQUhWQTY1N1IiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6OTZvOG1qYkpfZkM3R05OQjhFeEZGUHJXay01amtaMlA5VVZlbUs0ZkxUNCIsInQiOjE3OTA5NDg1NjU2MTcsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
