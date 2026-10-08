# Frontend development

Use a supported Node.js LTS release that meets [package.json](package.json)
(Node.js >=20). Run these commands from `hmem-server/frontend`:

```sh
npm ci
npm run test:browser:install
npm test
npm run dev
```

The browser install command provisions the Chromium revision required by the
locked Playwright dependency. Vite serves development on port 3000 and proxies
API and WebSocket traffic to the server on port 8420.

The workspace Timeline shows one horizontal lifecycle graph with entity and
Create, Complete, Delete, Archive, and Cancel toggles. Its default window is the
last 30 days with weekly buckets. Window and bucket choices persist per workspace.
All time starts at the earliest lifecycle event and the client follows bounded
bucket pages without changing the selected bucket size. Displayed timestamps use
the database timestamp's represented date and time, formatted `YYYY-MM-DD HH:MM`.

Observation cards show the bound current reviewed revision when available, or
the immutable creation revision for legacy observations. The expanded revision
value has a compact History disclosure with copyable SHAs and recorded dates.
Open history follows sequential pages of 25 records in recorded order; collapsing
stops continuation, and a failed page offers an explicit Retry.

All and Subject mode switches preserve the current physical scroll position,
including newer scrolling while results load. Observation and Project card
surfaces are left aligned with a responsive maximum outer width of 78rem; their
controls and the lifecycle graph retain the full available width.

```sh
npm run build
npm run preview
```

The build replaces `hmem-server/static`. Use the
[live integration guide](tests-js/README.md) for an isolated API and the
[performance guide](perf/README.md) for qualification commands and evidence rules.
Focused browser scripts are listed in [package.json](package.json). Conditional
and populated Observation tests need the native test harness and configured
PostgreSQL tools; their isolated fixtures expire within ten minutes and remove
their browser, server, PostgreSQL process, and sandbox. The conditional script
rebuilds the harness; the populated script uses the installed harness, which must
be relinked after snapshot migrations. Navigation checks include 320 CSS-pixel
reflow and enlarged text; they do not qualify native browser zoom or assistive
technology behavior. Local bridge/layout tests do not replace production browser
and performance acceptance.

## Configuration

`window.HMEM_CONFIG` is loaded before Elm starts and overrides the build-time
`VITE_HMEM_API_URL`, `VITE_HMEM_WS_URL`, and `VITE_HMEM_AUTH_MODE` values. Without
API or WebSocket overrides, the app uses its origin and `/api/v1/ws`.
See the [container runtime contract](../../config/container-runtime-contract.md#browser-runtime-config-strategy)
for the non-secret runtime fields and [authentication](../../auth.md) for login,
token storage, and CSRF setup. Never bake secrets into assets or runtime config.

Set `HMEM_CONFIG.workspaceSnapshotProfile` before startup to `full_v1` for complete
workspace snapshots. The default is `workspace_shell_v1`; no other value is
accepted. The setting is read once and does not change global snapshots or REST
pagination. Reload always requests a fresh snapshot.

## Editing and navigation gotchas

- One dirty or saving Observation draft survives row, tab, and Back navigation
  within the workspace. Return to draft, Save draft, and Discard draft remain
  available. Editing another observation returns to that draft. Leaving the
  workspace requires saving or discarding; discard cannot interrupt a save.
  Reload, closing the tab, permission/session revocation, and authoritative
  deletion can retire the draft. Drafts are never persisted in browser storage.
- Saves use the version captured when editing began. A competing write preserves
  the draft and shows a conflict. **Keep my draft** adopts the latest version for
  the next conditional save; **Use latest version** replaces the draft. If a
  delayed reply disagrees with an observed version, these choices wait for a
  current-version check. A failed check retains the draft; retrying a retained
  version can conflict again. Workspace, subjects, Git SHA, and creation time
  remain immutable.
- **Apply filters** submits filter inputs; **Match files** submits trimmed,
  deduplicated concrete paths and filter inputs. Wildcards are rejected. Refresh,
  save, delete, and retries reuse the applied query. Unapplied filters pause
  paging until Apply or Revert; unapplied path input leaves prior matches active.
  A selected Subject remains locked while filtering its exact results.
- Back and shared links restore applied queries, paths, and selection, with fresh
  requests. Drafts, cached pages, and disclosures stay local. URLs exceeding
  4096 UTF-8 bytes keep the query active on the page but replace it with a bounded
  marker; Back, reload, or sharing that marker restores defaults with a notice.
  Malformed versioned links also restore defaults with a notice.
- Failed page, detail, count, or live-refresh requests offer Retry and retain
  available results. Live refresh uses the submitted query and does not submit
  filter drafts. Offset pagination is not a snapshot of concurrent writes.
- Observation arrows open details and return to the originating card when it
  still exists; otherwise they return to results. Full text remains selectable,
  and **Copy full content** preserves stored line endings. UUID, path, glob, and
  SHA copy controls copy the complete value even when abbreviated.

The hierarchy and Observation views use scrolling viewports; cached results
remain reachable even when their rows are not mounted. Root **Load more** is
explicit; expanded hierarchy branches continue automatically. Failed or
incomplete branches retain Retry and do not claim completion. Interactive
viewport loading is separate from canonical snapshot/replay, which can still
traverse a full workspace in pages. The
[navigation decision](../../architecture/adrai/decisions/R01M/R01M48140QPDRRS113KC3AERB1H--keep-interactive-workspace-navigation-bounded-and-stale-response.decision.md)
defines the transport and rendering limits.

Project filters preserve the current scroll position while replacement rows load;
the final content can clamp the position at its bottom. Subtasks share their
parent task's enclosure while each mounted row remains independently measured.

**Show empty projects** is checked by default and saved per workspace. Unchecking
it requires a matching task anywhere in the subtree; matching ancestors remain
visible. Workspace group disclosure is saved globally. Workspace admins can
soft-delete a workspace after confirmation; selected-workspace deletion returns
home and retains its contents. There is no MCP workspace-delete tool.

The **Administration** tab requires explicit workspace admin access; implicit
local superadmin sessions retain the hidden-tab presentation. Failed membership
writes keep the form and list. Controls stay unavailable through the write and
authorization recheck; a failed recheck offers **Retry session**.

The performance harness hashes this README as a source input. Historical
qualification covers its recorded source/assets and measurement phase; it does
not qualify later documentation revisions. Preserve historical baselines and
receipts when changing this file.
