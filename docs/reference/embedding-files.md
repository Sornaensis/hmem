# Embedding file reference

Have your external producer read each exported Observation and write one JSON
result per line to `embedding-results.ndjson`. Each result must contain
**only** these fields:

| Field | Value |
| --- | --- |
| `format_version` | Preserve the exported value (`2`). |
| `observation_id`, `workspace_id` | Preserve the exported IDs. |
| `content_fingerprint`, `space_fingerprint` | Preserve both exported values. |
| `embedding` | Exactly 1536 finite numbers generated from the exported input. |

Do not copy the export-only `git_sha`, `subjects`, or `content` fields into
result records, or recalculate the fingerprint. Duplicate Observation IDs,
lines over 1 MiB, and inputs over 100000 records are rejected.
