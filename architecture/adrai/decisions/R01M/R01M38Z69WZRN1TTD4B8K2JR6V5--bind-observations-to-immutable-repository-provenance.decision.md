+++
schema = "adrai/decision/v1"
adr = "A01M38Z69R241CHNFSBT53227BK"
record = "R01M38Z69WZRN1TTD4B8K2JR6V5"
title = "Bind Observations to immutable repository provenance"
summary = "Fix workspace, Git SHA, and ordered subjects at creation while allowing content corrections."
domains = ["observation-provenance"]
+++

Context: An Observation can be corrected after creation while its repository identity must remain unambiguous. This record describes the current implementation; the historical reason for the choice is not established by these sources.

Observed decision: Bind each Observation to an active repository workspace, a lowercase 40-character Git SHA, and an ordered set of 1 to 256 canonical repository-relative file or glob subjects. The database stores subjects with ordinals, seals the set after creation, and rejects later subject, workspace, or Git SHA changes. The normal update changes content only. It clears any stored embedding and its space fingerprint; database triggers refresh full-text search from content and ordered subjects.

Consequences: Correcting a subject, workspace, or revision requires a new Observation, preserving the original record's provenance. A content correction remains possible, but an embedding made from the old content cannot be reused silently and search text follows the new content.

Evidence:
- database.md: documents the immutable provenance tuple, canonical subject rules, content-only update, embedding invalidation, and search-vector behavior.
- hmem-server/migrations/V020__replace_memories_with_observations.sql: creates the workspace and Git SHA columns and validates the SHA shape.
- hmem-server/migrations/V021__observation_subject_sets.sql: defines ordered canonical subject rows, subject-set and provenance guards, and search-vector triggers.
- hmem-core/src/HMem/DB/Observation.hs: creates Observations from workspace, normalized subjects, Git SHA, and content; content updates clear an existing embedding and its fingerprint.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjUyODVmYWE0YWM5N2NlNjIyNDExOTgxNjQyNWQyY2Y5MjkxOGZhZTAiLCJpIjoic2hhMjU2OkhSLWhSWXNLbzdtRTNnSlRRSndUa3pPdkZVbUhhMUh4Y0tNMTVzXzJLX1UiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zOFo2OVdaUk4xVFRENEI4SzJKUjZWNSIsIm9wIjoiTzAxTTM4WjY5V1pSTjFUVEQ0QjhLMkpSNlY1IiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6T1lHbDkxY2ttZkFENVNsLWtUTXJVbmhUeU9NWjJPYjZ1MFRMclhFbmEzVSIsInQiOjE3OTAyMjg3Njg2NzEsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
