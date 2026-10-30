# Transcript occurrence time

Canonical session `last_activity_at` is the greatest valid eligible occurrence
in the complete committed transcript. Equal instants choose the later raw record
offset. Older appends and exact replay preserve the maximum. Source order and
Activity group header times remain separate presentation rules.

Version 1 accepts an outer `timestamp`, or `time` when the former is absent,
null, false, numeric zero or an empty string (the shared legacy `timestamp || time` rule),
as an RFC3339 string with a four-digit year 0001–9999, valid calendar, seconds
00–59 and explicit `Z` or `±HH:MM` offset. Lowercase `t`/`z` is accepted. Offsets
have hours 00–23 and minutes 00–59; the resulting UTC year must remain 0001–9999.
Fractions are truncated to PostgreSQL microseconds. Numeric epochs have no
supported unit and are unknown, as are local/invalid dates. A nonempty invalid
outer timestamp does not fall through to `time`; nested timestamps, IDs and
receipt clocks are never searched. Arrays, objects, true and nonzero numeric
outer timestamps are truthy but invalid time values, so cannot select `time`.
Numeric truthiness uses JavaScript's IEEE754 Double value: positive/negative
underflow becomes zero and falls through; nonzero subnormals and overflow remain
truthy. This selection rule never changes raw numeric payloads or their meaning.

Eligible native occurrences are:

- Codex `response_item` messages containing visible text/thinking/tool blocks,
  function/custom tool calls and outputs, and visible/encrypted reasoning.
  `event_msg` agent/user messages and reasoning require content remaining after
  JavaScript `String.trim`, using their truthy `message || text` selection;
  other meaningful event payloads are Activity events. Token counts,
  `turn_aborted`, `task_started` and `task_complete` remain hidden.
- Claude `assistant`/`user` message envelopes with supported visible text,
  thinking, encrypted thinking, tool-use or tool-result blocks.
- Copilot `user.message`, `assistant.message` and `assistant.reasoning` with
  visible content, and tool execution starts/completions. Message tool requests
  count independently of execution. Known hook/command/permission failures count
  when their native `success` is false.
- pi schema 3 message entries (also headerless supported windows): visible
  user/assistant/system content, displayed custom messages, tool results,
  branch/compaction summaries and bash execution; displayed `custom_message`,
  `compaction`, `branch_summary` and `context_edit` entries. Unsupported schema
  versions and hidden native controls are excluded. The one schema scalar is
  carried between scan windows.

Generic text objects, unsupported notices, empty content, ephemeral progress,
native housekeeping and unknown envelopes are not occurrences. Meaningful
Codex fallback detection uses the viewer's finite 256-value/16-level inspection:
the whole outer event starts at depth zero. Content keys, nonempty trimmed
strings, numbers and nonempty arrays qualify; ambiguous
large/deep structures qualify; discriminator/boolean-only markers do not.
The finite budget follows `Object.keys` order and LIFO traversal, including
numeric-key ordering and duplicate keys' first positions/final values. Trimming
uses ECMAScript whitespace: U+FEFF is empty, while U+0085 and U+200B stay visible.
Copilot native events require an object `data` envelope; malformed arrays and
scalars remain unsupported notices and cannot supply occurrence authority.
Before ordinary JSON decoding, a lexical pass replaces containers below depth
32 with an ambiguous-content marker in the interpretation buffer. This bounds
decoder recursion while preserving the viewer's shallower ambiguity rule and
the original outer fields. Stored and downloaded evidence remains exact.
Paired Codex display envelopes remain source occurrences for the maximum;
display pairing does not rewrite the canonical evidence or timestamp extraction.

`session_occurrences` keeps version, exact processed byte cursor, maximum time,
winning source offset, pi version and at most one 1 MiB pending record. Its
public value is null until the current version reaches the exact committed
cursor with no pending record. Active appends advance an exact prior checkpoint
in their transaction; historical gaps remain pending for reconciliation. Parsing
the bounded append happens before the session row lock. A finite EOF record is
consumed once, and a later separator-only append cannot count it twice.

An independent occurrence worker starts and stops with the application's archive
worker lifetime, including the ordinary native and harness entrypoints. Archive
backfill/reclamation settings do not disable occurrence correction. One worker
rotates session UUIDs, advances at most 1 MiB per turn and keeps no connection or
transaction between turns. It uses the canonical raw-first reader and verified
gzip generations after reclamation. Checkpoints survive restart. Failures leave
the candidate pending for the next sweep. Session row locking and version/cursor
CAS prevent stale writers from replacing newer work; append/seal races never
publish a partial prefix as current authority. Sealing alone changes no content
cursor and needs no evidence rewrite.

The two application worker children share a finite five-second drain budget.
Both cancellation/join attempts run even when the first fails. Multiple failures
retain every actual child capability and its original exception in one receipt;
retrying that receipt re-drains each retained owner before dependent resources
can be released. Single failed joins retain their established receipt behavior.

Completion publishes the ordinary fixed session/workspace metadata invalidation,
without payloads or audit entries. Inline append uses its existing invalidation;
an identical checkpoint retry adds no event. Existing active, imported and sealed sessions
therefore gain corrected values after processing and canonical refresh. Creation,
heartbeats, polling, runtime declarations, task links, import completion and
termination never become occurrence time. Live/Quiet and abandoned-session
sealing continue to use `last_seen_at`.
