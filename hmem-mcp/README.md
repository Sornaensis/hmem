# hmem MCP

The MCP bridge lets agents use hmem observations, projects and tasks.
Keep hmem-server running, then configure your MCP client to launch:

```sh
hmem-mcp --server-url http://127.0.0.1:8420
```

After initialization, declare identity before using ordinary tools:

```json
{"workspace_id":null,"identity":{"role":"Developer","model-id":"codex:gpt-6.1-sol:high"}}
```

Use `workspace_list` or `workspace_register`, then call `set_workspace` again
with the chosen UUID and complete identity. A fresh MCP process needs a new
declaration. Shared installations also need a service token; see
[authentication](../auth.md#service-and-mcp-tokens).

See the [MCP reference](reference.md) for context selection, validation and errors.
