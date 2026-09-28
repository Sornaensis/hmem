+++
schema = "adrai/decision/v1"
adr = "A01M3KDGY4K4GHNPGBKTY3FMM2H"
record = "R01M3KDGY90G486EP30SDT18ANG"
title = "Match Observation subjects against canonical repository paths"
summary = "Use a restricted, case-sensitive file and glob grammar with matching Haskell and PostgreSQL behavior and no repository lookup."
domains = ["observation-matching"]
+++

**Context**

An Observation can cover multiple repository subjects, including file globs. Clients need to ask which Observations cover concrete changed paths without depending on the repository checkout or on differing host glob libraries. The provenance ADR defines the immutable subject set; it does not define path-matching semantics.

**Decision**

Accept canonical, repository-relative, forward-slash concrete paths as match inputs. A file subject matches by exact text. A glob subject uses only `*` for zero or more non-slash characters, `?` for one non-slash character, and `**` as a complete path component for zero or more directory components. Matching is case-sensitive; dotfiles are ordinary components. Reject character classes, braces, escapes, embedded `**`, absolute paths, and non-canonical segments.

Evaluate the provided concrete paths against the stored subjects with OR semantics. Return each matching Observation once, with matched paths in caller order and matched subjects in stored order. Do not inspect the filesystem, expand a Git tree, or accept caller globs. Keep the Haskell reference matcher and PostgreSQL matcher aligned through a shared behavior corpus.

**Consequences**

Callers get deterministic path coverage independent of checkout and platform. The intentionally small grammar excludes some familiar shell-glob syntax. Two match implementations must remain behaviorally aligned and tested whenever the contract changes.

**Evidence**

- `database.md`: canonical path rules, restricted grammar, matching request and evidence ordering.
- `hmem-core/src/HMem/Types.hs`: normalization, validation, and pure path matcher.
- `hmem-server/migrations/V021__observation_subject_sets.sql`: database constraints and matching functions.
- `hmem-core/src/HMem/DB/Observation.hs`: Observation match query.
- `hmem-core/test/HMem/ObservationSubjectMatchCorpus.hs`, `hmem-core/test/HMem/TypesSpec.hs`, and `hmem-core/test/HMem/DB/TestHarnessSpec.hs`: common Haskell/SQL cases.

<!-- @adrai:eyJhIjp7ImkiOiJjb2RleCIsImsiOiJsbG0ifSwiYiI6IjZkYmY5NDU5ZjlkYTc4NjJlOTZjZWViNGZkNWZiODRhYzhmZjAxMjYiLCJpIjoic2hhMjU2OjhfNDBxclR0TkR3bThfdjkyaERramI4LXFjMDFvZEx0dE5sdzFTcVBDd3MiLCJrIjoiZGVjaXNpb24uY3JlYXRlIiwibyI6IlIwMU0zS0RHWTkwRzQ4NkVQMzBTRFQxOEFORyIsIm9wIjoiTzAxTTNLREdZOTBHNDg2RVAzMFNEVDE4QU5HIiwiciI6Im1hc3RlciIsInMiOiJzaGEyNTY6d1pkczgyakhobDZpc05LZjAzejBSSEkyRFRFZk41STFYVXdGWDZ6OHBqUSIsInQiOjE3OTA1NzkzNDE2MDAsInYiOjEsIngiOiJhZHJhaS8xLjAuMCJ9 -->
