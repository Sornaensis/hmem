# Native TEI GPU qualification

For maintainers reproducing task `aa30a81c-2adf-4f54-b1eb-cfeaa39bac77`.
Operator installation is in [Docker deployment](../../docker.md); production
limits are in the [GPU contract](../../config/managed-embedding-gpu-contract.md).

The [foundation contract](../../hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json)
locks the SM120 TEI 1.9.3 image, original 20-file model snapshot, source inputs,
artifact root, and byte/time/output/disk/resource limits. The paths below are
contract-bound historical inputs and outputs; do not substitute arbitrary roots.

## Preparation and validation order

Run from the repository root with Python and Docker Desktop available:

```text
python -m unittest discover -s scripts/managed-embedding-gpu -p "test_*.py"
python scripts/managed-embedding-gpu/prepare_image.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json
python scripts/managed-embedding-gpu/run_tei_startup.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probe hmem-server/test/fixtures/embedding-gpu-viability/startup-probe-v1.json
```

Preparation authenticates the OCI manifests/layers, source archive, compatibility
files, and full model snapshot; it creates the verified model tree and records
the actual local image. `preparation-report-v1.json` proves input authentication
and image presence, not CUDA access or inference. The startup probe separately
checks the selected GPU, `/info`, EOS tokenization, and a normalized 1536-vector.

Prepare the independent reference image from the
[39-wheel lock](reference-requirements.lock) and authenticated metadata:

```text
python scripts/managed-embedding-gpu/prepare_reference_image.py --metadata hmem-server/test/fixtures/embedding-gpu-viability/reference-wheel-metadata-v1.json --metadata-provenance hmem-server/test/fixtures/embedding-gpu-viability/reference-wheel-size-provenance-v1.json --requirements scripts/managed-embedding-gpu/reference-requirements.lock --dockerfile scripts/managed-embedding-gpu/Dockerfile.reference-cuda --driver scripts/managed-embedding-gpu/reference_cuda.py --generation v4
```

Each locked wheel is fetched at most once, size/hash-verified, and installed
offline with `--require-hashes`. The
[qualification record](../../hmem-server/test/fixtures/embedding-gpu-viability/gpu-foundation-qualification-v1.json)
binds the derived image and its separate BuildKit digest. The
[reference driver](reference_cuda.py) uses authenticated original model code and
the accepted `original-sdpa-math-cuda-f16-v3` method; production TEI uses Candle.

Run the reference supervisor, then TEI validation:

```text
python scripts/managed-embedding-gpu/run_reference_validation.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probes hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json --output D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\reference-output\reference-v4.json
python scripts/managed-embedding-gpu/run_foundation_validation.py --contract hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json --probes hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json --reference D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\reference-output\reference-v4.json
```

The reference output must be create-new under the contract's `reference-output`
directory. TEI validation requires both the passed result and its successful
`.execution.json` attestation, issued only after authenticated container absence.
A failure sidecar records the original error and cleanup outcome and cannot
satisfy that prerequisite.

All model processes are serialized. Supervisors authenticate image, GPU,
ownership labels, mounts, host caps, and container IDs; use offline loopback
without publishing host ports; and bound output, execution, and cleanup. They
continuously monitor VRAM and stop the owned container to preserve a 2 GiB
display-memory reserve. Host memory caps do not cap VRAM. Fresh headroom checks
remain required. Reference cleanup must finish before TEI starts.

Validation checks exact token IDs, cosine/L2/coordinate/unit-norm thresholds,
singleton/repeat/batch/concurrent ordering, and the wrong-prompt negative. It also
tests real 32,768-token inference and the exact 32,769-token rejection without
truncation. Read-only request-body mounts avoid command-line limits.

## Evidence and limits

[Golden vectors](../../hmem-server/test/fixtures/embedding-gpu-viability/reference-golden-v1.json)
bind all four probes to the accepted reference result, execution attestation,
driver, image, model, and method. The qualification JSON is the downstream
authority for numerical results, cleanup, timings, and recommendations. Complete
immutable reports remain under the declared artifact root, bound by path,
length, and SHA-256; preserve that existing evidence.

The qualified RTX 5090 run recorded 51.03 s startup, 0.32–0.39 s compact
singletons, 0.47 s for four items, 0.50 s for two simultaneous mixed batches, and
2.08 s for 32,768 tokens. Peak observed VRAM was 8,724 MiB, not a hard limit.
Recommendations are 120 s startup, 30 s compact requests, and 300 s full-context
requests; use the GPU contract's admission limits.

Independent numerical parity covers four probes through 2,048 tokens. The
full-context run proves practical TEI inference and length rejection, not
independent full-context parity. hmem accepts at most 32,767 fully formatted
UTF-8 bytes. Earlier eager-float32, failed TEI-comparison, and weight-rounding
results remain separate historical evidence. There is no CPU or download fallback.
