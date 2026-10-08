# Historical ONNX CPU experiment

This experiment is **failed and unqualified**. The
[recorded report](../../hmem-server/test/fixtures/embedding-onnx-viability/report.json)
has verdict `not_viable`: pinned TEI reached its 28 GiB cap during 32,768-token
warmup and was OOM-killed with exit 137 before readiness. Full inference and
dependent API/scheduler/over-limit checks did not run. Increasing the deadline
does not repair that failure. It applies to this graph/runtime/host, not every
possible CPU graph. Supported installation is in [Docker deployment](../../docker.md).

## Reproduction inputs

[source-lock.json](source-lock.json) fixes the original 20-file
`Alibaba-NLP/gte-Qwen2-1.5B-instruct` snapshot at revision
`1cad2ab3ff41c2671f34e135d29831368ee26b68`. Retrieve its URLs at build time,
verify every SHA-256 and relative path, and pass `verify_source_snapshot` before
executing model code. Serving/comparison runs are offline.

Build the linux/amd64 toolchain from the locked base and
[hashed Python closure](requirements.lock):

```text
docker build --platform linux/amd64 --pull=false --tag hmem-managed-embedding-onnx:aa30a81c scripts/managed-embedding-onnx
```

The report records the exact verified toolchain/base digests. Run the following
commands inside that image with `--network none`, a read-only root and
`/source`, `/scripts`, `/fixtures` mounts, bounded writable output mounts, and
explicit CPU, memory, swap, process, and thread limits. Mount this directory at
`/scripts` and the
[viability fixtures](../../hmem-server/test/fixtures/embedding-onnx-viability/corpus.json)
at `/fixtures`. Use a writable temporary `HF_HOME` for importing verified local
model code. The [report](../../hmem-server/test/fixtures/embedding-onnx-viability/report.json)
records the measured resource limits and evidence paths.

Capture the build-only audit and compare it to the tracked
[license audit](../../hmem-server/test/fixtures/embedding-onnx-viability/toolchain-license-audit.json)
using its report-bound SHA-256:

```text
python /scripts/audit_toolchain_licenses.py --requirements /scripts/requirements.lock --output /evidence/toolchain-license-audit.json --toolchain-image hmem-managed-embedding-onnx@sha256:8ed20f622337a91b4b0638bce91f78dca5ff5f64bd4a9ee8b95d4f10e91c5bbe --base-image python@sha256:2856e6af199e8128161abd320575eb9b341f3b76f017b5d0c9cd364f60d8a050
```

Run export in two empty artifact roots and compare their inventories:

```text
python /scripts/export_model.py --source /source --output /output --threads 8
python /scripts/verify_graph.py --graph /output/model.onnx --report /output/graph-report.json
python /scripts/make_serving_tree.py --source /source --export /output --output /serving --inventory /evidence/candidate-inventory.json
python /scripts/reference_oracle.py --source /source --corpus /fixtures/corpus.json --thresholds /fixtures/thresholds.json --output /oracle --threads 8
python /scripts/compare_onnx.py --graph /output/model.onnx --oracle /oracle/oracle.npz --thresholds /fixtures/thresholds.json --report /output/ort-report.json --threads 8
```

The graph uses fp32, opset 17, dynamic batch/sequence axes, `use_cache=false`,
and explicit `is_causal=true`. Native TEI FlashQwen2 uses the config's
`is_causal=false`. This unresolved attention difference prevents promotion of
the graph or a shared embedding-space claim. The exact-hash guard in
[common.py](common.py) suppresses only Transformers' false unconditional
`flash_attn` scanner requirement; it does not change executed model code.

## Offline TEI probe

Use the exact CPU image in the report:
`ghcr.io/huggingface/text-embeddings-inference:cpu-1.9.3@sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07`,
linux/amd64, no network, and the serving tree read-only at `/model`. Launch with:

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

Only after readiness, run the client with `--network container:<tei-name>`:

```text
python /scripts/smoke_tei.py --url http://127.0.0.1:80 --metrics-url http://127.0.0.1:9000 --source /source --corpus /fixtures/corpus.json --thresholds /fixtures/thresholds.json --oracle /oracle/oracle.npz --report /evidence/tei-smoke-report.json
```

The eleven short inputs use ordered batches of at most eight. The 32,769-token
negative requires all token records and final EOS 151643 from `/tokenize`, then
an independent length-specific 422 from `/embed` with no truncation. No default
prompt is permitted. The actual preload path is `/usr/local/libfakeintel.so`;
the historical production lock records `usr/local/lib/libfakeintel.so`.
Record actual loader mapping without weakening that lock.

The [threshold fixture](../../hmem-server/test/fixtures/embedding-onnx-viability/thresholds.json)
was frozen before results. Each coordinate requires
`abs(diff) <= atol + rtol*abs(reference)`; normalized vectors also have a cosine
distance limit. Relative error is diagnostic. The graph verifier bounds optional
external-data offsets/lengths, rejects partial overlaps, and hashes every file.

## Redistribution limits

Model and TEI source are Apache-2.0. The exact-image audit also records wheel,
Python, and Debian component licenses/notices, including GPL/LGPL terms.
Redistribution must retain applicable notices and satisfy source-correspondence
requirements. Review remains incomplete for the audit's
`unresolved_redistribution_items` lacking machine-readable license expressions;
missing metadata does not establish incompatibility. This experiment does not
commit or redistribute weights, packages, or the base filesystem, and does not
promote an ONNX artifact into the production manifest.
