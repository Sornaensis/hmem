# Document preview worker

This package produces static document previews inside a restricted Linux container.
The host launcher uses Python's standard library and Docker; it does not load the
document decoders into the application process.

Build from the repository root with Docker Desktop's Linux engine, or a Linux
Docker engine with cgroup v2 and seccomp:

```text
docker build -t hmem-preview-worker hmem-preview-worker
docker image inspect --format {{.Id}} hmem-preview-worker
```

Configure the launcher with that immutable `sha256:…` image ID. Mutable image tags
are rejected for preview execution. See the [worker reference](../docs/reference/document-preview-worker.md)
for the two-stage protocol, deployment boundary and validation commands.
