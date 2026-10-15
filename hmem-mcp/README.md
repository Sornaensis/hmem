# MCP workspace and identity context

After the MCP initialization handshake, call `set_workspace` before ordinary
tools. Both `workspace_id` and `identity` are required:

```json
{"workspace_id":null,"identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

A null workspace allows `workspace_list` and `workspace_register` before selecting
a workspace. Select one by calling `set_workspace` again with its UUID and the
complete identity:

```json
{"workspace_id":"00000000-0000-0000-0000-000000000001","identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

These are `set_workspace` arguments; in a raw JSON-RPC `tools/call`, put them in
`params.arguments` and use `params.name: "set_workspace"`. Scoped tools can then
omit `workspace_id`; an explicit value on an ordinary call takes precedence.
Missing workspace values still fail scoped
tool validation when no workspace is selected.

The role is nonblank and limited to 128 UTF-8 bytes; `model-id` is limited to 256
UTF-8 bytes. Both limits apply before trimming. Model-id has exactly three
nonempty `harness:model:reasoning` components.
Components use ASCII letters, digits, `.`, `_`, `/`, or `-`. Outer whitespace is
trimmed, control and format characters are rejected, and identity accepts only
`role` and `model-id`. Invalid declarations leave the previous workspace and
identity unchanged. `get_workspace` and successful `set_workspace` return the
accepted values, without claiming a server-resolved actor ID.

Setting a null workspace clears only the workspace and accepts the supplied
identity. Repeating `initialize` preserves the context. A fresh MCP process starts
without either value and requires its own declaration. Protocol/tool discovery
and the local context controls are available before declaration; all HTTP
forwarding requires an accepted identity, including calls with explicit workspace
IDs and global workspace tools.

Each accepted ordinary request captures the complete context. Context controls
drain earlier queued and active requests before changing it; ordinary requests
within an epoch remain concurrent. Both HTTP forwarding paths send one
`X-HMem-Bot-Identity` header containing the accepted JSON identity. Declaration is
attribution metadata: existing bearer credentials and MCP provenance are forwarded
unchanged, and local tokenless operation requires no new credentials. Launch
with `--no-auth` to suppress configured credential forwarding explicitly. Shared
servers still require their ordinary credentials and grants.

## Errors and compatibility

Before declaration, ordinary calls return a tool result with `isError: true` and
instructions to call `set_workspace`; supplying `workspace_id` on that call does
not bypass startup. Missing identity, unknown keys, invalid workspace values, or
invalid role/model values also return `isError: true` without changing either
accepted context value. Older clients that sent only a workspace must include
identity on every `set_workspace` call, including when clearing with null.

Identity remains accepted when the workspace is null; scoped calls still need a
workspace, while discovery/registration can proceed. `get_workspace` reports both
values as null before the first declaration. Restarting the MCP process requires
a new declaration; repeating `initialize` is not a reset.

Declarations are [self-reported attribution](../auth.md#declared-bot-identity).
The server derives a local persona only for implicit local bootstrap: the same
canonical role/model pair groups across sessions and workspaces. Credential bot
IDs, human identities, permissions, and stream audiences remain unchanged. New
records expose optional `bot_identity`; no historical attribution is inferred.
