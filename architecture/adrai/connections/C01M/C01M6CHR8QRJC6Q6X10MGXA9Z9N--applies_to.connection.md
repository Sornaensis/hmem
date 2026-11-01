+++
schema = "adrai/connection/v1"
connection = "C01M6CHR8QRJC6Q6X10MGXA9Z9N"
relation = "applies_to"
added = ["config/audit-maintenance.yaml.example", "docs/reference/audit-maintenance.md", "hmem-core/cbits/audit_*", "hmem-core/src/HMem/DB/AuditMaintenance.hs", "hmem-core/src/HMem/DB/AuditMaintenance/**", "hmem-server/migrations/V056__audit_maintenance_controls.sql", "hmem-server/migrations/V057__audit_maintenance_operations.sql", "hmem-server/src/HMem/Server/*AuditMaintenance*.hs", "hmem-server/src/HMem/Server/AuditMaintenance/**", "hmem-server/test/HMem/Server/AuditMaintenance/**", "hmem-server/test/HMem/Server/AuditMaintenanceSpec.hs", "scripts/validation/AuditSpace.hs", "scripts/validation/audit-space.ps1"]
applies_to = ["config/audit-maintenance.yaml.example", "database.md", "docs/guides/agent-sessions.md", "docs/reference/audit-maintenance.md", "docs/reference/schema.md", "docs/reference/upgrades.md", "hmem-core/cbits/audit_*", "hmem-core/src/HMem/DB/Audit.hs", "hmem-core/src/HMem/DB/AuditMaintenance.hs", "hmem-core/src/HMem/DB/AuditMaintenance/**", "hmem-core/test/HMem/DB/AgentSessionSpec.hs", "hmem-server/migrations/V002__soft_deletes_audit_and_integrity.sql", "hmem-server/migrations/V020__replace_memories_with_observations.sql", "hmem-server/migrations/V052__session_lifecycle_audit.sql", "hmem-server/migrations/V056__audit_maintenance_controls.sql", "hmem-server/migrations/V057__audit_maintenance_operations.sql", "hmem-server/src/HMem/Server/*AuditMaintenance*.hs", "hmem-server/src/HMem/Server/API.hs", "hmem-server/src/HMem/Server/AuditMaintenance/**", "hmem-server/test/HMem/Server/AuditMaintenance/**", "hmem-server/test/HMem/Server/AuditMaintenanceSpec.hs", "hmem-server/test/HMem/Server/SessionReaperSpec.hs", "scripts/validation/AuditSpace.hs", "scripts/validation/audit-space.ps1"]
change = "expand"
parent_connections = ["C01M649HHFFT15VA2ZT7E3FRBYE"]
removed = []
subject_adr = "A01M39RCB0NC51143VRNQDG4R5Y"
+++

Apply audit preservation and ownership policy to guarded maintenance

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0iLCJtIjoiZ3B0LTYuMS1zb2wifSwiYiI6IjU2NWViODA3MjUxODdmZGZjMDJmZTRmNTYzMTVkMjMyNWUwMDU2MDIiLCJrIjoic2NvcGUuZXhwYW5kIiwibyI6IkMwMU02Q0hSOFFSSkM2UTZYMTBNR1hBOVo5TiIsIm9wIjoiTzAxTTZDSFI4UVJKQzZRNlgxME1HWEE5WjlOIiwicCI6WyJDMDFNNjQ5SEhGRlQxNVZBMlpUN0UzRlJCWUUiXSwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6bG1sTUktWjhvQks2WUZFMy1iQldqWElGUER0NmNsbHdaWk9wZXI4aEYyUSIsInQiOjE3OTM1NzAxMjA0NDAsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
