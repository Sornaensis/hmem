# Pinned GTE-Qwen2 fp32 ONNX viability recipe

This directory contains the bounded experiment for
`Alibaba-NLP/gte-Qwen2-1.5B-instruct` revision
`1cad2ab3ff41c2671f34e135d29831368ee26b68` and the pinned TEI 1.9.3 CPU
image. It creates evidence outside the repository and does not promote an ONNX
artifact into the production manifest.

The source directory must contain exactly the 20 files in `source-lock.json`.
Retrieve each `source-lock.json` URL at build time, verify its SHA-256, and place
it at the listed relative path. Model code is executed only after this complete
local snapshot passes `verify_source_snapshot`; serving and comparison commands
run without network access.

Build the toolchain for linux/amd64 from the locked base and hashed Python
closure:

```text
docker build --platform linux/amd64 --pull=false --tag hmem-managed-embedding-onnx:aa30a81c scripts/managed-embedding-onnx
```

The verified experiment image is
`hmem-managed-embedding-onnx@sha256:8ed20f622337a91b4b0638bce91f78dca5ff5f64bd4a9ee8b95d4f10e91c5bbe`.
Its base is the linux/amd64 manifest
`python@sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050`.
`requirements.lock` pins the complete install closure and package hashes.

Capture the build-only license evidence from that exact image, then compare it
to the tracked audit SHA-256
`995db3fd3fadb5e9acf6fd652253a95d0432db3247be4f3ea9b7e727f186ed87`:

```text
python /scripts/audit_toolchain_licenses.py --requirements /scripts/requirements.lock --output /evidence/toolchain-license-audit.json --toolchain-image hmem-managed-embedding-onnx@sha256:8ed20f622337a91b4b0638bce91f78dca5ff5f64bd4a9ee8b95d4f10e91c5bbe --base-image python@sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050
```

Create two empty artifact roots and run the same export command against each.
The executed custom model source is hash-checked. The graph uses fp32, opset 17,
dynamic batch and sequence axes, `use_cache=false`, and explicit
`is_causal=true`, matching the model card's published call default. This choice
is attention-qualified: the pinned native TEI FlashQwen2 backend instead passes
the configuration's `is_causal=false`. Until that authority issue is resolved,
the experiment cannot establish a shared embedding space or promote its graph.
The pinned module guards its `flash_attn` imports with
`is_flash_attn_2_available()`, but Transformers 4.41.2's remote-code scanner
treats them as unconditional. `permit_guarded_flash_attention_import` suppresses
only that scanner dependency for a file with the exact pinned SHA-256; the
executed CPU model remains unchanged and uses no flash-attn library.

```text
python /scripts/export_model.py --source /source --output /output --threads 8
python /scripts/verify_graph.py --graph /output/model.onnx --report /output/graph-report.json
python /scripts/make_serving_tree.py --source /source --export /output --output /serving --inventory /evidence/candidate-inventory.json
python /scripts/reference_oracle.py --source /source --corpus /fixtures/corpus.json --thresholds /fixtures/thresholds.json --output /oracle --threads 8
python /scripts/compare_onnx.py --graph /output/model.onnx --oracle /oracle/oracle.npz --thresholds /fixtures/thresholds.json --report /output/ort-report.json --threads 8
```

Run every command in the locked image with `--network none`, a read-only root,
read-only `/source`, `/scripts`, and `/fixtures` mounts, a writable bounded
output mount, and explicit CPU, memory, swap, process, and thread limits. Use a
writable temporary `HF_HOME` so Transformers can import the already verified
local custom model module.

The TEI proof uses the exact image
`ghcr.io/huggingface/text-embeddings-inference:cpu-1.9.3@sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07`
on linux/amd64. Start it with no network and the derived serving tree mounted
read-only at `/model`. A client container using `--network container:<tei-name>`
shares only that offline loopback namespace and runs `smoke_tei.py` with
`--url http://127.0.0.1:80 --metrics-url http://127.0.0.1:9000` (also the
script defaults). The smoke client splits the eleven short corpus items into
ordered request batches of at most eight, matching the fixed client ceiling.
The fixed router arguments are:

```text
--model-id /model
--served-model-name Alibaba-NLP/gte-Qwen2-1.5B-instruct
--hostname 127.0.0.1
--port 80
--prometheus-port 9000
--revision 1cad2ab3ff41c2671f34e135d29831368ee26b68
--auto-truncate false
--max-batch-tokens 32768
--max-batch-requests 8
--max-client-batch-size 8
--dtype float32
--pooling last-token
```

After readiness, run the client in the TEI container's offline network
namespace with the launch ports explicitly matched:

```text
python /scripts/smoke_tei.py --url http://127.0.0.1:80 --metrics-url http://127.0.0.1:9000 --source /source --corpus /fixtures/corpus.json --thresholds /fixtures/thresholds.json --oracle /oracle/oracle.npz --report /evidence/tei-smoke-report.json
```

For the 32,769-token negative case, the pinned `/tokenize` route is expected to
return all 32,769 token records with final EOS 151643; its source path does not
apply the embedding token-count limit. The `/embed` route must independently
return the exact length-specific 422 rejection with truncation disabled.

Do not pass a default prompt. In the actual image, the preload path is
`/usr/local/libfakeintel.so`; the production lock currently records the
extracted-layer path as `usr/local/lib/libfakeintel.so`. The runtime test must
record the actual loader mapping without changing or silently weakening the
production manifest.

The numerical thresholds in the fixture were frozen at
`2026-09-06T12:09:37.492Z`, before any oracle or held-out result, with SHA-256
`3ee04181139c7a72623bd7e42f011555321e61f05b96ba52ce942f2b0a12319e`.
Each coordinate uses `abs(diff) <= atol + rtol*abs(reference)`. Maximum relative
error is diagnostic; normalized vectors also have a cosine-distance limit.

ONNX external-data `offset` and `length` fields are optional. The verifier
records whether each field was declared, computes the standard effective range
(offset zero and length to end-of-file when omitted), bounds every effective
range, rejects partial overlaps, and hashes every referenced file.

The model and TEI source are Apache-2.0. `toolchain-license-audit.json` records
the license metadata and shipped license/notice-file hashes for every locked
wheel, the Python runtime license, and the installed Debian package copyright
inventory in the exact build image. Apache-2.0, BSD, MIT, MPL-2.0, PSF, GPL and
LGPL family terms occur in that evidence. Redistribution therefore requires
retaining the applicable copyright/license/NOTICE material and satisfying the
copyleft source-correspondence terms for redistributed base-image components.
The audit leaves redistribution review incomplete because some wheel metadata
omits a machine-readable license expression; those exact packages are listed in
`unresolved_redistribution_items` for review against their captured license
fields, classifiers, and files. That metadata absence is not evidence of license
incompatibility. This experiment does not redistribute or commit the packages,
base-image filesystem, or weights.
