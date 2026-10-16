# Schema reference

## Declared attribution (V039)

`V039__declared_bot_identity.sql` adds nullable `bot_identity` JSONB to
`audit_log` and `observation_revision_events`. The validated object contains only
`role` and `model-id`. Transaction-local settings carry the effective actor and
optional declaration to generic, Observation, and project-pause audit writers,
revision events, Timeline reads, and the durable outbox. Pooled transactions
explicitly clear absent actor/declaration settings so requests cannot inherit
an earlier caller's attribution. Authorization continues to use the original
Principal and its grants.

Observation events and `current_provenance` use the same canonical projection:
omit only the new `bot_identity` key when absent, retain existing mandatory
nullable actor fields, and require exact event/projection equality. Append-only
history, contiguous sequences, and the current head remain enforced. Outbox
actors keep `user`, `service`, and `system`; a bot maps to `service`, and genuinely
absent request context maps to `system`.

The migration owns its transaction and version-39 ledger entry. Existing history,
audit records, current projections, and stored outbox envelopes are not rewritten
or assigned inferred declarations. This additive metadata does not change
snapshot envelope versions or require a new resync of otherwise valid tokens.
Older payloads remain readable. New same-declaration local personas group across
workspaces/sessions; credential-backed bots keep their credential actor IDs
instead of substituting authorization grant-user IDs. See
[the trust contract](../../auth.md#declared-bot-identity).

## Due-field removal (V038)

`V038__remove_task_due.sql` drops `idx_tasks_due` and `tasks.due_at`, including
values on soft-deleted tasks. Subtasks use the same table; projects have no due
column. Fresh installs run the ordered migration chain and reach this schema.
The migration owns one transaction containing snapshot retirement, schema
changes, and its version-38 ledger entry. A failure rolls these changes back;
after correcting the failure, retry through the migration runner. An already
registered V038 is skipped.

Only saved workspace `full_v1` snapshot sessions containing a task item with a
`due_at` key (even null) are retired, together with their linked resume tokens,
including successor tokens, and cascading cached items/page tokens. Clients
must obtain a fresh snapshot when those tokens are refused. Compatible global,
workspace-shell, and empty full snapshots remain valid. Fresh task snapshot
envelopes retain schema version 1 and omit due fields.

Historical audit values and persisted outbox envelopes are retained unchanged;
they can still contain obsolete due data. Task IDs, unrelated values and
timestamps are preserved. There is no down migration or recovery of removed
deadline values from current task rows. Follow the
[coordinated upgrade and backup rollback procedure](../../docs/reference/upgrades.md#upgrading-across-v038).

Older versioned migrations retain their due-column references to describe and
upgrade the historical schema. Audit/outbox fixtures may retain obsolete due
values as compatibility evidence. Operational retry times, job eligibility,
leases, token expiry and request timeouts are separate from removed planning
deadlines and remain supported.
