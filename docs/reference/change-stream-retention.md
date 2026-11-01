# Change-stream replay retention

The outbox defaults to 86,400 seconds (one day), with an 82,800-second
resume-token lifetime and a 300-second snapshot lifetime. Missing YAML blocks
and fields use these defaults. Explicit YAML and environment settings remain
supported; validation clamps snapshot and resume lifetimes so their sum is
strictly below retention.

```yaml
change_stream:
  retention_seconds: 86400
  resume_token_ttl_seconds: 82800
  snapshot_session_ttl_seconds: 300
```

The environment equivalents are `HMEM_CHANGE_STREAM_OUTBOX_RETENTION_SECONDS`,
`HMEM_CHANGE_STREAM_RESUME_TOKEN_TTL_SECONDS` and
`HMEM_CHANGE_STREAM_SNAPSHOT_SESSION_TTL_SECONDS`.

V055 adds bounded cursor-prefix pruning without changing table geometry. A
worker becomes eligible at startup and after 60 elapsed monotonic seconds.
Backlog drains continue on later worker turns. Each turn handles at most four
scope transactions, each deleting
at most 1,000 rows with a 100 ms lock timeout and 500 ms statement timeout.
Only one scope is locked per transaction. Scope identity rotates across turns,
including failed scopes, so a busy workspace cannot monopolize retention.
The older internal multi-scope helper retains ascending workspace UUID then
global locking and removes at most 1,000 rows per call.

Deletion and replay-floor advancement commit together. A cutoff is exclusive:
records exactly at the cutoff remain. A retained record stops prefix deletion;
later old timestamps cannot make retention skip it. Cancellation, lock failure
or statement timeout cannot commit a new floor without its deleted prefix.
Expired snapshot/token cleanup retains its existing policy and has finite
statement budgets. It does not delete audit history, session evidence or
planning recovery data.

The `change_stream_retention` server log exposes deleted rows, a capped expired
row lower bound (1,001 means at least 1,001), oldest expired age and errors.
Scan/token-cleanup errors are explicit; unknown backlog is not zero. Repeated
identical errors are counted in memory rather than logged on every drain turn.
For `hmem-ctl` installations, stderr is captured in
`<installation>/logs/hmem-server-stdio.log`; foreground servers emit these
lines on stderr. Status is emitted at idle minute checks and state transitions.
`readChangeStreamMaintenance` provides the worker's compact latest attempt,
successful turn, failure count and error state to in-process callers. An old
expired row behind a retained prefix can remain in the lower bound until that
prefix itself expires. These figures describe logical replay cleanup, not
allocated-file reclamation. The existing delivery loop may delay later turns.

Tokens issued under a former, longer lifetime do not extend replay retention.
Expired or pruned replay requires a new authorized canonical snapshot; valid
retained replay continues to recheck scope, audience and authorization epoch.
Raising retention later cannot restore events already pruned. Restore a prior
database backup into a separate recovery environment if historical events are
required; changing this setting alone is insufficient.

See the [controlled upgrade procedure](upgrades.md#bounded-replay-retention-v055)
before deploying the matching server and migration.
