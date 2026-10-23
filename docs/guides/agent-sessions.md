# Agent sessions and evidence

Open a workspace's **Sessions** tab to inspect its agent activity. The list shows
50 sessions at a time. Use Lifecycle and Heartbeat filters to find active,
terminated, fresh or stale sessions, and Older/Newer to move through pages.
Heartbeat becomes stale after 30 seconds without a server acknowledgement; it
does not mean that the session has terminated or that a local uploader has
stopped. The connection indicator describes live change delivery separately.

Select a session to see its creator, committed byte/record counts, linked tasks
and transcript. A direct link uses `#tab=sessions&session=<session UUID>` in the
workspace URL. Linked tasks open their existing task focus; that task's Sessions
and evidence panel links back to its sessions. A later task association can also
refer to evidence that was already terminated.

The transcript follows the tail by default. **Pause** preserves your readable
position while the new-byte count increases. **Follow tail** catches up, and
**Load older window** reads a bounded earlier section. The viewer shows at most
100 records and 2 MiB at once. A window starting inside a record omits that
leading fragment with a notice; older windows recover earlier complete records.
Provider-specific objects, arrays and scalar JSON remain available through
**Original JSON**, with their byte positions. Reading or pausing never seals a
session.

A terminated session displays its immutable final byte count and SHA-256.
**Save original evidence** opens your browser's file picker and streams the
authorized canonical bytes to that file. The application verifies the final
count and SHA-256 before completing the save. Keep the page and workspace open
until verification completes; cancelling or changing authority aborts the save.
Browsers without the required writable-file capability show that limitation.
The bounded MCP `session_evidence` tool remains available for retrieving original
evidence in chunks. Neither path turns the displayed, bounded record window into
the source of the saved evidence.

Uploading and termination are independent operations. A stopped uploader can
leave an active session, and an explicit termination seals only the server's
committed prefix. Inspect the receipt rather than assuming local file EOF or task
status established evidence completeness.
